{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Instances
  (
  )
where

import           Ledger.Core
  ( Addr(..)
  , Epoch(..)
  , Hash(..)
  , Lovelace(..)
  , Owner(..)
  , Sig(..)
  , VKey(..)
  , VKeyGenesis(..)
  )
import           Ledger.Delegation
  ( DCert(..)
  )
import           Ledger.UTxO
  ( Tx(..)
  , TxId(..)
  , TxIn(..)
  , TxOut(..)
  , TxWits(..)
  , Wit(..)
  )
import           Test.Goblin

--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

instance Goblin Bool Addr where
instance Goblin Bool Epoch where
  tinker (Epoch w) = Epoch <$> tinker w
  conjure = Epoch <$> conjure
instance Goblin Bool Hash where
  tinker (Hash i) = Hash <$> tinker i
  conjure = Hash <$> conjure
instance Goblin Bool Lovelace where
  tinker (Lovelace i) = Lovelace <$> tinker i
  conjure = Lovelace <$> conjure
instance Goblin Bool Owner where
  tinker (Owner n) = Owner <$> tinker n
  conjure = Owner <$> conjure
instance Goblin Bool (Sig Tx) where
instance Goblin Bool (Sig VKeyGenesis) where
instance Goblin Bool VKey where
  tinker (VKey o) = VKey <$> tinker o
  conjure = VKey <$> conjure
instance Goblin Bool VKeyGenesis where
  tinker (VKeyGenesis k) = VKeyGenesis <$> tinker k
  conjure = VKeyGenesis <$> conjure


instance Goblin Bool Tx where
instance Goblin Bool TxId where
  tinker (TxId h) = TxId <$> tinker h
  conjure = TxId <$> conjure
instance Goblin Bool TxIn where
instance Goblin Bool TxOut where
instance Goblin Bool TxWits where
instance Goblin Bool Wit where

instance Goblin Bool DCert where
