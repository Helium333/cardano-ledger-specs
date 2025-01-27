{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Bhead
  ( BHEAD
  )
where

import qualified Data.Set                      as Set

import           BaseTypes
import           BlockChain
import           Keys
import           LedgerState
import           PParams
import           Slot

import           STS.NewEpoch
import           STS.Rupd

import           Control.State.Transition

data BHEAD hashAlgo dsignAlgo kesAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => STS (BHEAD hashAlgo dsignAlgo kesAlgo)
 where
  type State (BHEAD hashAlgo dsignAlgo kesAlgo)
    = NewEpochState hashAlgo dsignAlgo
  type Signal (BHEAD hashAlgo dsignAlgo kesAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo
  type Environment (BHEAD hashAlgo dsignAlgo kesAlgo)
    = (Seed, Set.Set (VKeyGenesis dsignAlgo))
  data PredicateFailure (BHEAD hashAlgo dsignAlgo kesAlgo)
    = HeaderSizeTooLargeBHEAD
    | BlockSizeTooLargeBHEAD
    | NewEpochFailure (PredicateFailure (NEWEPOCH hashAlgo dsignAlgo))
    | RupdFailure (PredicateFailure (RUPD hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

bheadTransition
  :: forall hashAlgo dsignAlgo kesAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => TransitionRule (BHEAD hashAlgo dsignAlgo kesAlgo)
bheadTransition = do
  TRC ((etaC, gkeys), nes@(NewEpochState _ _ bprev _ es ru _ _), bh@(BHeader bhb _)) <-
    judgmentContext
  let slot                = bheaderSlot bhb
  let EpochState _ _ _ pp = es

  fromIntegral (bHeaderSize bh) < _maxBHSize pp ?! HeaderSizeTooLargeBHEAD
  fromIntegral (hBbsize bhb) < _maxBBSize pp ?! BlockSizeTooLargeBHEAD

  nes' <- trans @(NEWEPOCH hashAlgo dsignAlgo)
    $ TRC ((NewEpochEnv etaC slot gkeys), nes, epochFromSlot slot)

  ru' <- trans @(RUPD hashAlgo dsignAlgo) $ TRC ((bprev, es), ru, slot)
  let nes'' = nes' { nesRu = ru' }
  pure nes''

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => Embed (NEWEPOCH hashAlgo dsignAlgo) (BHEAD hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = NewEpochFailure

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => Embed (RUPD hashAlgo dsignAlgo) (BHEAD hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = RupdFailure
