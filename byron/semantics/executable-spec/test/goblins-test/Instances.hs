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
  tinker gen = do
    gen' <- tinker ((\(Addr w) -> w) <$> gen)
    pure (Addr <$> gen')
  conjure = Addr <$$> conjure
instance Goblin Bool Epoch where
  tinker gen = do
    gen' <- tinker ((\(Epoch w) -> w) <$> gen)
    pure (Epoch <$> gen')
  conjure = Epoch <$$> conjure
instance Goblin Bool Hash where
  tinker gen = do
    gen' <- tinker ((\(Hash w) -> w) <$> gen)
    pure (Hash <$> gen')
  conjure = Hash <$$> conjure
instance Goblin Bool Lovelace where
  tinker gen = do
    gen' <- tinker ((\(Lovelace w) -> w) <$> gen)
    pure (Lovelace <$> gen')
  conjure = Lovelace <$$> conjure
instance Goblin Bool Owner where
  tinker gen = do
    gen' <- tinker ((\(Owner w) -> w) <$> gen)
    pure (Owner <$> gen')
  conjure = Owner <$$> conjure
instance Goblin Bool (Sig Tx) where
  tinker gen = do
    genX <- tinker ((\(Sig x _) -> x) <$> gen)
    genY <- tinker ((\(Sig _ y) -> y) <$> gen)
    pure (Sig <$> genX <*> genY)
  conjure = (\x y -> Sig <$> x <*> y) <$> conjure <*> conjure
instance Goblin Bool (Sig VKeyGenesis) where
  tinker gen = do
    genX <- tinker ((\(Sig x _) -> x) <$> gen)
    genY <- tinker ((\(Sig _ y) -> y) <$> gen)
    pure (Sig <$> genX <*> genY)
  conjure = (\x y -> Sig <$> x <*> y) <$> conjure <*> conjure
instance Goblin Bool VKey where
  tinker gen = do
    gen' <- tinker ((\(VKey w) -> w) <$> gen)
    pure (VKey <$> gen')
  conjure = VKey <$$> conjure
instance Goblin Bool VKeyGenesis where
  tinker gen = do
    gen' <- tinker ((\(VKeyGenesis w) -> w) <$> gen)
    pure (VKeyGenesis <$> gen')
  conjure = VKeyGenesis <$$> conjure


instance Goblin Bool Tx where
  tinker gen = do
    genX <- tinker ((\(Tx x _) -> x) <$> gen)
    genY <- tinker ((\(Tx _ y) -> y) <$> gen)
    pure (Tx <$> genX <*> genY)
  conjure = (\x y -> Tx <$> x <*> y) <$> conjure <*> conjure
instance Goblin Bool TxId where
  tinker gen = do
    gen' <- tinker ((\(TxId w) -> w) <$> gen)
    pure (TxId <$> gen')
  conjure = TxId <$$> conjure
instance Goblin Bool TxIn where
  tinker gen = do
    genX <- tinker ((\(TxIn x _) -> x) <$> gen)
    genY <- tinker ((\(TxIn _ y) -> y) <$> gen)
    pure (TxIn <$> genX <*> genY)
  conjure = (\x y -> TxIn <$> x <*> y) <$> conjure <*> conjure
instance Goblin Bool TxOut where
  tinker gen = do
    genX <- tinker ((\(TxOut x _) -> x) <$> gen)
    genY <- tinker ((\(TxOut _ y) -> y) <$> gen)
    pure (TxOut <$> genX <*> genY)
  conjure = (\x y -> TxOut <$> x <*> y) <$> conjure <*> conjure
instance Goblin Bool TxWits where
  tinker gen = do
    genX <- tinker ((\(TxWits x _) -> x) <$> gen)
    genY <- tinker ((\(TxWits _ y) -> y) <$> gen)
    pure (TxWits <$> genX <*> genY)
  conjure = (\x y -> TxWits <$> x <*> y) <$> conjure <*> conjure
instance Goblin Bool Wit where
  tinker gen = do
    genX <- tinker ((\(Wit x _) -> x) <$> gen)
    genY <- tinker ((\(Wit _ y) -> y) <$> gen)
    pure (Wit <$> genX <*> genY)
  conjure = (\x y -> Wit <$> x <*> y) <$> conjure <*> conjure

instance Goblin Bool DCert where
  tinker gen = do
    genX <- tinker ((\(DCert x _ _ _) -> x) <$> gen)
    genY <- tinker ((\(DCert _ y _ _) -> y) <$> gen)
    genZ <- tinker ((\(DCert _ _ z _) -> z) <$> gen)
    genW <- tinker ((\(DCert _ _ _ w) -> w) <$> gen)
    pure (DCert <$> genX <*> genY <*> genZ <*> genW)
  conjure = (\x y z w -> DCert <$> x <*> y <*> z *> w)
        <$> conjure <*> conjure <*> conjure <*> conjure


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
