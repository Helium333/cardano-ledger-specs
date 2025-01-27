{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module STS.Overlay
  ( OVERLAY
  )
where

import qualified Data.Map.Strict               as Map
import           Numeric.Natural                ( Natural )

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           OCert
import           PParams
import           Slot

import           STS.Ocert

import           Control.State.Transition

data OVERLAY hashAlgo dsignAlgo kesAlgo

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => STS (OVERLAY hashAlgo dsignAlgo kesAlgo)
 where
  type State (OVERLAY hashAlgo dsignAlgo kesAlgo)
    = Map.Map (KeyHash hashAlgo dsignAlgo) Natural

  type Signal (OVERLAY hashAlgo dsignAlgo kesAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo

  type Environment (OVERLAY hashAlgo dsignAlgo kesAlgo) =
    ( PParams
    , Map.Map Slot (Maybe (VKeyGenesis dsignAlgo))
    , Seed
    , PoolDistr hashAlgo dsignAlgo
    , Dms dsignAlgo
    )

  data PredicateFailure (OVERLAY hashAlgo dsignAlgo kesAlgo)
    = NotPraosLeaderOVERLAY
    | NotActiveSlotOVERLAY
    | WrongGenesisColdKeyOVERLAY
    | NoGenesisStakingOVERLAY
    | OcertFailure (PredicateFailure (OCERT hashAlgo dsignAlgo kesAlgo))
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [overlayTransition]

overlayTransition
  :: forall hashAlgo dsignAlgo kesAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
     )
  => TransitionRule (OVERLAY hashAlgo dsignAlgo kesAlgo)
overlayTransition = do
  TRC ((pp, osched, eta0, pd, Dms dms), cs, bh@(BHeader bhb _)) <-
    judgmentContext
  let gkey'' = Map.lookup (bheaderSlot bhb) osched
  let vk     = bvkcold bhb
  case gkey'' of
    Nothing -> do
      vrfChecks eta0 pd (_activeSlotCoeff pp) bhb ?! NotPraosLeaderOVERLAY
      cs' <- trans @(OCERT hashAlgo dsignAlgo kesAlgo) $ TRC ((), cs, bh)
      pure cs'
    Just gkey' -> do
      case gkey' of
        Nothing   -> failBecause NotActiveSlotOVERLAY
        Just gkey -> do
          let dmsKey' = Map.lookup gkey dms
          case dmsKey' of
            Nothing     -> failBecause NoGenesisStakingOVERLAY
            Just dmsKey -> vk == dmsKey ?! WrongGenesisColdKeyOVERLAY
      cs' <- trans @(OCERT hashAlgo dsignAlgo kesAlgo) $ TRC ((), cs, bh)
      pure cs'

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed (OCERT hashAlgo dsignAlgo kesAlgo) (OVERLAY hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = OcertFailure
