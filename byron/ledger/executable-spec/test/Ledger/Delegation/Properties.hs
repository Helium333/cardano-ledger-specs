{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Properties of the delegation traces induced by the transition systems
-- associated with this aspect of the ledger.
module Ledger.Delegation.Properties
  ( dcertsAreTriggered
  , dcertsAreNotReplayed
  , rejectDupSchedDelegs
  , tracesAreClassified
  , dblockTracesAreClassified
  , relevantCasesAreCovered
  , onlyValidSignalsAreGenerated
  )
where

import           Control.Arrow (first, (&&&), (***))
import           Control.Lens (makeLenses, to, view, (&), (.~), (^.))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.List (foldl', last, nub)
import           Data.List.Unique (count, repeated)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Hedgehog (Gen, MonadTest, Property, assert, cover, forAll, property, success,
                     withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.State.Transition (Embed, Environment, IRC (IRC), PredicateFailure, STS,
                     Signal, State, TRC (TRC), applySTS, initialRules, judgmentContext, trans,
                     transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasSizeInfo, HasTrace,
                     TraceProfile (TraceProfile), classifySize, classifyTraceLength, envGen,
                     failures, isTrivial, nonTrivialTrace, proportionOfInvalidSignals,
                     proportionOfValidSignals, ratio, sigGen, suchThatLastState, trace,
                     traceLengthsAreClassified, traceWithProfile)
import qualified Control.State.Transition.Generator as TransitionGenerator
import           Control.State.Transition.Trace (Trace, TraceOrder (OldestFirst), lastState,
                     preStatesAndSignals, traceEnv, traceLength, traceSignals)
import           Ledger.Core (Epoch (Epoch), Owner (Owner), Sig (Sig), Slot, SlotCount (SlotCount),
                     VKey (VKey), VKeyGenesis, addSlot, mkVKeyGenesis, owner, signWithGenesisKey,
                     unSlot, unSlotCount)
import           Ledger.Delegation (DCert, DELEG, DIState (DIState),
                     DSEnv (DSEnv, _dSEnvEpoch, _dSEnvK), DSState (DSState),
                     DState (DState, _dStateDelegationMap, _dStateLastDelegation),
                     PredicateFailure (IsAlreadyScheduled, SDelegFailure, SDelegSFailure),
                     delegate, delegationMap, delegator, depoch, epoch, liveAfter, mkDCert,
                     scheduledDelegations, slot, _dIStateDelegationMap,
                     _dIStateKeyEpochDelegations, _dIStateLastDelegation,
                     _dIStateScheduledDelegations, _dSEnvAllowedDelegators,
                     _dSStateKeyEpochDelegations, _dSStateScheduledDelegations)

import           Ledger.Core.Generators (epochGen, slotGen, vkGen)
import qualified Ledger.Core.Generators as CoreGen
import           Ledger.GlobalParams (slotsPerEpoch)

--------------------------------------------------------------------------------
-- Delegation certification triggering tests
--------------------------------------------------------------------------------

-- | Initial state for the ADELEG and ADELEGS systems
initADelegsState :: DState
initADelegsState = DState
  { _dStateDelegationMap  = Bimap.empty
  , _dStateLastDelegation = Map.empty
  }

-- | Initial state for the ADELEG and ADELEGS systems
initSDelegsState :: DSState
initSDelegsState = DSState
  { _dSStateScheduledDelegations = []
  , _dSStateKeyEpochDelegations  = Set.empty
  }

-- | Initial state for the DELEG system
initialDIState :: DIState
initialDIState = DIState
  { _dIStateDelegationMap  = _dStateDelegationMap initADelegsState
  , _dIStateLastDelegation = _dStateLastDelegation initADelegsState
  , _dIStateScheduledDelegations = initSDelegsState ^. scheduledDelegations
  , _dIStateKeyEpochDelegations  = _dSStateKeyEpochDelegations initSDelegsState
  }

-- | Delegation blocks. Simple blockchain to test delegation.
data DBLOCK

-- | A delegation block.
data DBlock
  = DBlock
    { _blockSlot  :: Slot
    , _blockCerts :: [DCert]
    }
  deriving (Show, Eq)

makeLenses ''DBlock

-- | This corresponds to a state-transition rule where blocks with increasing
-- slot-numbers are produced.
instance STS DBLOCK where
  type Environment DBLOCK = DSEnv -- The initial environment is only used to bootstrap the initial state.
  type State DBLOCK = (DSEnv, DIState)
  type Signal DBLOCK = DBlock

  data PredicateFailure DBLOCK
    = DPF (PredicateFailure DELEG)
    | NotIncreasingBlockSlot
    | InvalidDelegationCertificate -- We need this to be able to use the trace profile.
    deriving (Eq, Show)

  initialRules
    = [ do
          IRC env <- judgmentContext
          pure (env, initialDIState)
      ]

  transitionRules
    = [ do
          TRC (_, (env, st), dblock) <- judgmentContext
          let nextSlot = dblock ^. blockSlot
          env ^. slot < nextSlot ?! NotIncreasingBlockSlot
          stNext <- trans @DELEG $ TRC (env, st, dblock ^. blockCerts)
          let nextEpoch = if _dSEnvK env == 0
                          then 0
                          else Epoch $ unSlot nextSlot `div` slotsPerEpoch (_dSEnvK env)
          return (env & slot .~ nextSlot
                      & epoch .~ nextEpoch
                 , stNext)
      ]

instance Embed DELEG DBLOCK where
  wrapFailed = DPF

-- | Check that all the delegation certificates in the trace were correctly
-- applied.
dcertsAreTriggeredInTrace :: MonadTest m => Trace DBLOCK -> m ()
dcertsAreTriggeredInTrace tr
  = lastDms === trExpectedDms
  where
    -- | Delegation map at the final state.
    lastDms = st ^. delegationMap

    -- | Delegation map what we'd expect to see judging by the delegation
    -- certificates in the trace' signals.
    trExpectedDms
      = expectedDms lastSlot
                    ((fromIntegral . unSlotCount . liveAfter) (_dSEnvK env))
                    slotsAndDcerts

    (env, st) = lastState tr

    -- | Last slot that was considered for an activation.
    lastSlot :: Int
    lastSlot = fst . last $ slotsAndDcerts

    -- | Slots in which each block was applied. This is simply the result of
    -- pairing the slot number in the pre-state of a signal, with the signal
    -- itself.
    --
    -- We make use of integers, since negative numbers are quite handy when
    -- computing the slot at which a given certificate should have been
    -- activated (see 'expectedDms' and 'activationSlot').
    slotsAndDcerts :: [(Int, DBlock)]
    slotsAndDcerts
      = first (view (to fst . slot . to unSlot . to fromIntegral))
      <$> preStatesAndSignals OldestFirst tr

-- | Compute the expected delegation map after applying the sequence of
-- delegation certificates contained in the given blocks.
--
-- Delegation certificates are applied in the order they appear in the within a
-- block, and blocks are considered in the order they appear on the list passed
-- as parameter.
expectedDms
  :: Int
  -- ^ Last slot that should have been considered for certificate activation.
  -> Int
  -- ^ Delegation certificate liveness parameter.
  -> [(Int, DBlock)]
  -- ^ Delegation certificates to apply, and the slot at which these
  -- certificates where scheduled.
  -> Bimap VKeyGenesis VKey
expectedDms s d sbs =
  foldl' insertIfInjective Bimap.empty (fmap delegatorDelegate activeCerts)
  where
    -- | Insert the key-value pair in the map only if the value is not in the
    -- map already.
    insertIfInjective
      :: Bimap VKeyGenesis VKey
      -> (VKeyGenesis, VKey)
      -> Bimap VKeyGenesis VKey
    insertIfInjective m (k, v) =
      if Bimap.memberR v m
      then m
      else Bimap.insert k v m

    -- | Get the active certificates from each block, and concatenate them all
    -- together.
    activeCerts :: [DCert]
    activeCerts = concatMap _blockCerts activeBlocks

    -- | We keep all the blocks whose certificates should be active given the
    -- current slot.
    activeBlocks :: [DBlock]
    activeBlocks
      =  snd
     <$> filter ((<= activationSlot) . fst) sbs

    -- | Slot at which the certificates should have become active. If this
    -- number is negative that means that no certificate can be activated.
    activationSlot :: Int
    activationSlot = s - d

delegatorDelegate :: DCert -> (VKeyGenesis, VKey)
delegatorDelegate = delegator &&& delegate

-- | Check that there are no duplicated certificates in the trace.
dcertsAreNotReplayed :: Property
dcertsAreNotReplayed = withTests 300 $ property $ do
  let (thisTraceLength, step) = (1000, 100)
  sample <- forAll (traceWithProfile @DBLOCK thisTraceLength profile)
  classifyTraceLength sample thisTraceLength step
  dcertsAreNotReplayedInTrace sample
  where
    dcertsAreNotReplayedInTrace
      :: MonadTest m
      => Trace DBLOCK
      -> m ()
    dcertsAreNotReplayedInTrace traceSample
      = repeated traceDelegationCertificates === []
      where
        traceDelegationCertificates = traceSignals OldestFirst traceSample
                                    & fmap _blockCerts
                                    & concat
    profile
      = TraceProfile
        { proportionOfValidSignals = 95
        , proportionOfInvalidSignals = 5
        , failures = [(1, InvalidDelegationCertificate)]
        }

instance HasTrace DBLOCK where

  envGen
    chainLength
    = DSEnv
    <$> allowedDelegators
    -- We do not expect the current epoch to have an influence on the tests, so
    -- we chose a small value here.
    <*> epochGen 0 10
    -- As with epochs, the current slot should not have influence in the tests.
    <*> slotGen 0 10
    -- 2160 the value of @k@ used in production. However, delegation certificates are activated
    -- @2*k@ slots from the slot in which they are issued. This means that if we want to see
    -- delegation activations, we need to choose a small value for @k@ since we do not want to blow
    -- up the testing time by using large trace lengths.
    --
    -- Choosing a small @k@ value amounts to picking a large number of epochs. Given a trace length
    -- of @n@, if we have @10k@ slots per-epoch, we can have at most @n `div` 10@ epochs (by
    -- choosing @k == 1@).
    --
    <*> CoreGen.k chainLength (chainLength `div` 10)
    where
      -- We scale the number of delegators linearly up to twice the number of genesis keys we use in
      -- production. Factor 2 is chosen ad-hoc here.
      allowedDelegators = do
        n <- Gen.integral (Range.linear 0 13)
        pure $! Set.fromAscList $ mkVKeyGenesis <$> [0 .. n]

  sigGen (Just InvalidDelegationCertificate) _ (env, _st) =
    DBlock <$> nextSlotGen env <*> Gen.list (Range.constant 0 10) randomDCertGen
    where
      -- | Generate a random delegation certificate, which has a high probability of failing since
      -- we do not consider the current delegation state. So for instance, we could generate a
      -- delegation certificate for a genesis key that already delegated in this epoch.
      randomDCertGen :: Gen DCert
      randomDCertGen = do
        (vkg, vk, e) <- (,,) <$> vkgGen' <*> vkGen' <*> epochGen'
        pure $! mkDCert vkg (signWithGenesisKey vkg (vk, e)) vk e
        where
          vkgGen' = Gen.element $ Set.toList allowed
          allowed = _dSEnvAllowedDelegators env
          vkGen' = Gen.element $ VKey . Owner <$> [0 .. (2 * fromIntegral (length allowed))]
          epochGen' =  Epoch
                    .  fromIntegral -- We don't care about underflow. We want to generate large epochs anyway.
                    .  (fromIntegral n +)
                   <$> Gen.integral (Range.constant (-2 :: Int) 2)
            where Epoch n = _dSEnvEpoch env
  sigGen _ _ (env, st) =
    DBlock <$> nextSlotGen env <*> sigGen @DELEG Nothing env st


-- | Generate a next slot. We want the resulting trace to include a large number of epoch changes,
-- so we generate an epoch change with higher frequency.
nextSlotGen :: DSEnv -> Gen Slot
nextSlotGen env =
  incSlot <$> Gen.frequency
                [ (1, Gen.integral (Range.constant 1 10))
                , (2, pure $! slotsPerEpoch (_dSEnvK env))
                ]
  where
    incSlot c = (env ^.slot) `addSlot` SlotCount c

instance HasSizeInfo DBlock where
  isTrivial = null . view blockCerts

dcertsAreTriggered :: Property
dcertsAreTriggered = withTests 300 $ property $
  -- The number of tests was determined ad-hoc, since the default failed to
  -- uncover the presence of errors.
  forAll (nonTrivialTrace 1000) >>= dcertsAreTriggeredInTrace

dblockTracesAreClassified :: Property
dblockTracesAreClassified = withTests 200 $ property $ do
  let (tl, step) = (1000, 100)
  tr <- forAll (trace @DBLOCK tl)
  classifyTraceLength tr tl step
  -- Classify the traces by the total number of delegation certificates on
  -- them.
  classifySize "total dcerts" (traceDCerts tr) (fromIntegral . length) tl step
  success

-- | Extract the delegation certificates in the blocks, in the order they would
-- have been applied.
traceDCerts :: Trace DBLOCK -> [DCert]
traceDCerts tr = concat $ _blockCerts <$> traceSignals OldestFirst tr

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withTests 400 $ property $ do
  let tl = 1000
  tr <- forAll (trace @DBLOCK tl)

  -- 70% of the traces must contain are as many delegation certificates as
  -- blocks.
  cover 50
        "there are at least as many delegation certificates as blocks"
        (traceLength tr <= length (traceDCerts tr))

  -- 70% of the traces must contain at most 25% of blocks with empty delegation
  -- payload.
  cover 70
        "at most 25% of the blocks can contain empty delegation payload"
        (ratio emptyDelegationPayload tr <= 0.25)

  -- 50% of the traces must contain at least 30% of delegations to this epoch.
  cover 50
        "at least 30% of the certificates delegate in this epoch"
        (0.3 <= ratio thisEpochDelegations tr)

  -- 50% of the traces must contain at least 50% of delegations to the next
  -- epoch.
  cover 50
        "at least 50% of the certificates delegate in the next epoch"
        (0.5 <= ratio nextEpochDelegations tr)

  -- 80% of the traces must contain at least 30% of self-delegations.
  cover 80
       "at least 30% of the certificates self delegate"
       (0.3 <= ratio selfDelegations tr)

  -- 15% of the traces must contain at least 10% of delegations to the same
  -- delegate.
  cover 15
        "at least 5% of the certificates delegate to the same key"
        (0.05 <= ratio multipleDelegations tr)
  where
    selfDelegations :: Trace DBLOCK -> Int
    selfDelegations tr = length
                       $ filter idDeleg
                       $ fmap delegatorDelegate (traceDCerts tr)
      where
        idDeleg (vks, vk) = owner vks == owner vk

    -- Count the number of delegations to the same key in a given trace.
    multipleDelegations :: Trace DBLOCK -> Int
    multipleDelegations tr = -- Get the (delegator, delegate) pairs
                             fmap delegatorDelegate (traceDCerts tr)
                             -- Remove duplicated elements, since we're not
                             -- interested in the same genesis key delegating
                             -- to the same key, i.e. if we have more than one
                             -- @(vkg, vk)@, for the same genesis key @vkg@ and
                             -- key @vk@ we keep only one of them.
                           & nub
                             -- Keep the delegators. Since we applied nub
                             -- before, we know that if there are two keys in
                             -- the result of 'fmap snd' then we know for sure
                             -- that they were delegated by different keys.
                           & fmap snd
                             -- Count the occurrences. If we have more than one
                             -- occurrence of a key, then we know that it must
                             -- be because two different genesis keys delegated
                             -- to it.
                           & count
                           & filter ((2 <=) . snd)
                           & length

    emptyDelegationPayload  :: Trace DBLOCK -> Int
    emptyDelegationPayload tr =  _blockCerts <$> traceSignals OldestFirst tr
                              & filter null
                              & length

    thisEpochDelegations  :: Trace DBLOCK -> Int
    thisEpochDelegations tr = epochDelegationEpoch tr
                            & filter (uncurry (==))
                            & length

    -- Get the epoch in which the delegation certificates of the trace were
    -- applied, paired with the epoch of the delegation certificate.
    epochDelegationEpoch :: Trace DBLOCK -> [(Epoch, Epoch)]
    epochDelegationEpoch tr = preStatesAndSignals @DBLOCK OldestFirst tr
                            & fmap (_dSEnvEpoch . fst *** (fmap depoch . _blockCerts))
                            & fmap (\(e, es) -> zip (repeat e) es)
                            & concat


    nextEpochDelegations  :: Trace DBLOCK -> Int
    nextEpochDelegations tr =  epochDelegationEpoch tr
                            & filter (\(e0, e1) -> e0 + 1 == e1)
                            & length


onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @DBLOCK 100

--------------------------------------------------------------------------------
-- Properties related to the transition rules
--------------------------------------------------------------------------------

-- | Reject delegation certificates where a genesis key tries to delegate in
-- the same slot.
--
-- This property tries to generate a trace where the last state contains a
-- non-empty sequence of scheduled delegations. If such trace cannot be
-- generated, then the test will fail when the heap limit is reached, or
-- hedgehog gives up.
rejectDupSchedDelegs :: Property
rejectDupSchedDelegs = property $ do
  (tr, dcert) <- forAll $ do
    tr <- trace @DELEG 1000
          `suchThatLastState` (not . null . view scheduledDelegations)
    let vkS =
          case lastState tr ^. scheduledDelegations of
            (_, (res, _)):_ -> res
            _ -> error $  "This should not happen: "
                       ++ "tr is guaranteed to contain a non-empty sequence of scheduled delegations"
    vkD <- vkGen
    epo <- Epoch <$> Gen.integral (Range.linear 0 100)
    let dcert = mkDCert vkS (Sig (vkD, epo) (owner vkS)) vkD epo
    return (tr, dcert)
  let pfs = case applySTS (TRC (tr ^. traceEnv, lastState tr, [dcert])) of
        Left res -> res
        Right _ -> []
  assert $ SDelegSFailure (SDelegFailure IsAlreadyScheduled) `elem` concat pfs

-- | Classify the traces.
tracesAreClassified :: Property
tracesAreClassified = traceLengthsAreClassified @DELEG 1000 100
