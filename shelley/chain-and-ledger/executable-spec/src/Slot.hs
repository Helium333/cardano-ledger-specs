{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Slot
  ( Slot(..)
  , Duration(..)
  , (-*), (+*), (*-)
  , Epoch(..)
  -- conversion functions
  , slotFromEpoch
  , epochFromSlot
  , slotsPerEpoch
  , firstSlot
  -- conversion between Byron / Shelley
  , slotByronToShelley
  , slotShelleyToByron
  ) where

import           Data.Monoid             (Sum(..))
import           Numeric.Natural         (Natural)

import           Cardano.Binary          (ToCBOR)

import qualified Ledger.Core           as Byron (Slot(..))

-- |A Slot
newtype Slot = Slot Natural
  deriving (Show, Eq, Ord, Num, ToCBOR)
  deriving (Semigroup, Monoid) via (Sum Natural)

newtype Duration = Duration Natural
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum)
  deriving (Semigroup, Monoid) via (Sum Natural)

(-*) :: Slot -> Slot -> Duration
(Slot s) -* (Slot t) = Duration (if s > t then s - t else t - s)

(+*) :: Slot -> Duration -> Slot
(Slot s) +* (Duration d) = Slot (s + d)

-- | Subtract a duration from a slot
(*-) :: Slot -> Duration -> Slot
(Slot s) *- (Duration d) = Slot (if s > d then s - d else 0)

-- |An Epoch
newtype Epoch = Epoch Natural
  deriving (Show, Eq, Ord, ToCBOR)
  deriving (Semigroup, Monoid) via (Sum Natural)

slotFromEpoch :: Epoch -> Slot
slotFromEpoch (Epoch n) = Slot $ slotsPerEpoch * n

epochFromSlot :: Slot -> Epoch
epochFromSlot (Slot n) = Epoch $ n `div` slotsPerEpoch

firstSlot :: Epoch -> Slot
firstSlot = slotFromEpoch

-- | Hard coded global constant for number of slots per epoch
slotsPerEpoch :: Natural
slotsPerEpoch = 100

-- | Convert `Slot` data from Byron to Shelley, there should be a check that
-- Shelley slots fit into `Word64` used in Byron.
slotByronToShelley :: Byron.Slot -> Slot
slotByronToShelley (Byron.Slot s) = Slot $ fromIntegral s

slotShelleyToByron :: Slot -> Byron.Slot
slotShelleyToByron (Slot s) = Byron.Slot $ fromIntegral s
