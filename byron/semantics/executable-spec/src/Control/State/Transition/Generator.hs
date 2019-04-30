{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Generators for transition systems.
--
--   How should these work?
--   - We start with some initial environment.
--   - We start with some initial base state.
--   - We generate a stream of signals. These might be influenced by some running state
--   - We run each signal through
--
module Control.State.Transition.Generator
  ( HasTrace
  , initEnvGen
  , sigGen
  , trace
  , traceSuchThat
  , suchThatLastState
  , nonTrivialTrace
  , HasSizeInfo
  , isTrivial
  , sampleMaxTraceSize
  , randomTrace
  )
where

import Control.Monad (forM)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (Size(Size), unSize)

import Control.State.Transition
  ( Environment
  , IRC(IRC)
  , STS
  , Signal
  , State
  , TRC(TRC)
  , applySTS
  )
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , lastState
  , mkTrace
  , traceLength
  , traceSignals
  )

class STS s => HasTrace s where
  initEnvGen :: Gen (Environment s)

  sigGen :: Environment s -> State s -> Gen (Signal s)

  trace :: Gen (Trace s)
  trace = do
    env <- initEnvGen @s
    case applySTS @s (IRC env) of
      -- Hedgehog will give up if the generators fail to produce any valid
      -- initial state, hence we don't have a risk of entering an infinite
      -- recursion.
      Left _pf  -> trace
      -- Applying an initial rule with an environment and state will simply
      -- validate that state, so we do not care which state 'applySTS' returns.
      Right st -> genTrace env st (sigGen @s)

-- | Return a (valid) trace generator given an initial state, environment, and
-- signal generator.
genTrace
  :: forall s
   . STS s
  => Environment s
  -> State s
  -> (Environment s -> State s -> Gen (Signal s))
  -> Gen (Trace s)
genTrace env st aSigGen = Gen.sized $ \d -> mkTrace env st <$> go d st []
  where
    go d sti acc =
      Gen.frequency [ (5, return acc)
                    -- The probability of continue with the recursion depends
                    -- on the size parameter of the generator. Here the
                    -- constant factor is determined ad-hoc.
                    , (unSize d * 2, do
                          mStSig <- genSigSt @s env sti aSigGen
                          case mStSig of
                            Nothing ->
                              go d sti acc
                            Just (stNext, sig) ->
                              go d stNext ((stNext, sig): acc)
                      )
                    ]
  -- An alternate way to generate a trace of the size of the generator might
  -- be:
  --
  -- >>>  go 0 _   acc = return acc
  -- >>>  go d sti acc = do
  -- >>>    mStSig <- genSigSt @s env sti aSigGen
  -- >>>    case mStSig of
  -- >>>      Nothing ->
  -- >>>        go (d - 1) sti acc
  -- >>>      Just (stNext, sig) ->
  -- >>>        go (d - 1) stNext ((stNext, sig): acc)
  --

-- | Return a signal-and-ensuing-state generator, given an initial state,
-- environment and signal generator.
genSigSt
  :: forall s
   . STS s
  => Environment s
  -> State s
  -> (Environment s -> State s -> Gen (Signal s))
  -> Gen (Maybe (State s, Signal s))
genSigSt env st aSigGen = do
  sig <- aSigGen env st
  -- TODO: we might want to know why are we getting a given failure...
  case applySTS @s (TRC(env, st, sig)) of
    Left _ -> pure Nothing
    Right nextSt -> pure $ Just (nextSt, sig)

traceSuchThat
  :: forall s
   . HasTrace s
  => (Trace s -> Bool)
  -> Gen (Trace s)
traceSuchThat cond = Gen.filter cond (trace @s)

suchThatLastState
  :: forall s
   . Gen (Trace s)
  -> (State  s -> Bool)
  -> Gen (Trace s)
suchThatLastState traceGen cond = Gen.filter (cond . lastState) traceGen

-- | Generate a trace that contains at least one non-trivial signal. See
-- 'HasSizeInfo'.
nonTrivialTrace
  :: forall s
   . (HasTrace s, HasSizeInfo (Signal s))
  => Gen (Trace s)
nonTrivialTrace =
  Gen.filter (any (not . isTrivial) . traceSignals OldestFirst) trace

class HasSizeInfo sig where
  isTrivial :: sig -> Bool

instance HasSizeInfo [a] where
  isTrivial = null

--------------------------------------------------------------------------------
-- Trace sampling utilities
--------------------------------------------------------------------------------

-- | Sample the maximum trace size, given the generator size and number of
-- samples.
sampleMaxTraceSize
  :: forall s
   . HasTrace s
  => Int
  -- ^ Generator size
  -> Int
  -- ^ Number of samples to take
  -> IO Int
sampleMaxTraceSize d n =
  maximum <$>
    forM [0..n] (const $ traceLength <$> Gen.sample (Gen.resize (Size d) (trace @s)))

randomTrace
  :: forall s
   . HasTrace s
  => IO (Trace s)
randomTrace = Gen.sample trace

--------------------------------------------------------------------------------
-- Goblins-related functions
--------------------------------------------------------------------------------

-- | Progressive generator for STS systems, using
--   state threading underneath.
class STS a => ProgressiveGen a where
  -- | Underlying state for generation
  data GenState a :: *

data Bookkeeping s = Bookkeeping
  { bkGenState    :: GenState s
  , bkState       :: State s
  , bkEnvironment :: Environment s
  , bkTrace       :: [(Signal s, [PredicateFailure s])]
  } deriving Generic

-- | Generation monad
newtype GenM s a = GenM (GenT (MS.State (Bookkeeping s)) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "bkGenState" (GenState s))
    via Field "bkGenState" () (MonadState (GenT (MS.State (Bookkeeping s))))

deriving via Field "bkState" () (MonadState (GenT (MS.State (Bookkeeping s))))
  instance (st ~ State s) => HasState "bkState" st (GenM s)

deriving via Field "bkEnvironment" () (MonadState (GenT (MS.State (Bookkeeping s))))
  instance (env ~ Environment s) => HasState "bkEnvironment" env (GenM s)

deriving via WriterLog (Field "bkTrace" () (MonadState (GenT (MS.State (Bookkeeping s)))))
  instance (sig ~ Signal s) => HasWriter "bkTrace" [(sig, [PredicateFailure s])] (GenM s)

-- | Run the generator, given a generator for the environment and an initial generator state.
runGenM
  :: forall a s. STS s
  => Gen (Environment s)
  -> GenState s
  -> GenM s a
  -> Gen (a, Bookkeeping s)
runGenM envGen genState (GenM act) = do
  env <- envGen
  let initialState = fst . applyRuleIndifferently @s (IRC env) $ head initialRules
      bk = Bookkeeping genState initialState env []
      trick = ((,) <$> act <*> lift MS.get)
  hoist (return . flip MS.evalState bk) trick

-- | Step an STS generator, using a given signal generator.
stsStepGen
  :: forall s. ProgressiveGen s
     -- | Generator for signals
  => GenM s (Signal s)
     -- | Resulting state
  -> GenM s (State s)
stsStepGen sigGen = do
  env <- get @"bkEnvironment"
  st <- get @"bkState"
  signal <- sigGen
  -- We attempt to apply all (non-base) rules
  let
    jc :: TRC s
    jc = TRC (env, st, signal)
    (passed, failed) = partition (null . snd)
      $ applyRuleIndifferently @s jc <$> transitionRules
  st' <- case passed of
    [(st', _)] -> do
      tell @"bkTrace" [(signal, [])]
      return st'
    (st', _):_ -> do
      -- TODO More than one rule could be applied. This is bad.
      tell @"bkTrace" [(signal, [])]
      return st'
    [] -> do
      (st', pfs) <- GenM $ Gen.element failed-- TODO Why is `MonadGen` not immediately derivable for `GenM`?
      tell @"bkTrace" [(signal, pfs)]
      return st'
  put @"bkState" st'
  return st'
