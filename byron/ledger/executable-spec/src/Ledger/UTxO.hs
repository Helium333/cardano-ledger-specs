{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ledger.UTxO where

import           Data.AbstractSize (HasTypeReps, abstractSize)
import           Data.Hashable     (Hashable)
import qualified Data.Hashable     as H
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Maybe        (fromMaybe)
import           Data.TreeDiff.Class (ToExpr(..))
import           Data.TreeDiff.Expr (Expr(..))
import           Data.Typeable     (typeOf)
import           GHC.Generics      (Generic)
import           Numeric.Natural   (Natural)

import           Ledger.Core       hiding ((<|))
import           Ledger.Update     (PParams (PParams), _factorA, _factorB)
import           Test.Goblin

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Hashable)
  deriving anyclass (HasTypeReps)

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural
  deriving (Show, Eq, Ord, Generic, Hashable, HasTypeReps)

-- |The output of a UTxO.
data TxOut = TxOut { addr  :: Addr
                   , value :: Lovelace
                   } deriving (Show, Eq, Ord, Generic, Hashable, HasTypeReps)

-- |The unspent transaction outputs.
newtype UTxO = UTxO
  { unUTxO :: Map TxIn TxOut
  } deriving stock (Show)
    deriving newtype (Eq, Relation)

addValue :: TxOut -> Lovelace -> TxOut
addValue tx@TxOut{ value } d = tx { value = value + d }

-- | Construct a UTxO from initial TxOuts
fromTxOuts :: [TxOut] -> UTxO
fromTxOuts = UTxO . Map.fromList . fmap (\out -> (TxIn (mkId out) 0, out))
  where mkId = TxId . hash . addr

-- | A raw transaction
data Tx = Tx
  { inputs  :: [TxIn]
  , outputs :: [TxOut]
  } deriving (Eq, Show, Ord, Generic, Hashable, HasTypeReps)

txid :: Tx -> TxId
txid = TxId . hash

-- | Total value of a transaction.
txValue :: Tx -> Lovelace
txValue Tx { outputs } = sum $ fmap value outputs

-- |Compute the UTxO inputs of a transaction.
txins :: Tx -> [TxIn]
txins = inputs

-- |Compute the UTxO outputs of a transaction.
txouts :: Tx -> UTxO
txouts tx = UTxO $ Map.fromList
  [ (TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..] ]
  where transId = txid tx

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Lovelace
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where addValues b (TxOut _ a) = b <> a

instance Ledger.Core.HasHash Tx where
  hash = Hash . H.hash

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

pcMinFee :: PParams -> Tx -> Lovelace
pcMinFee PParams {_factorA = a, _factorB = b} tx
  = fromIntegral $ a + b * txsize tx

txsize :: Tx -> Int
txsize = abstractSize costs
  where costs = Map.fromList [ (typeOf (undefined :: TxIn) , 1)
                             , (typeOf (undefined :: TxOut), 1)
                             ]


---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey (Sig Tx)
  deriving (Show, Eq, Ord, Generic, Hashable, HasTypeReps)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits = TxWits
  { body      :: Tx
  , witnesses :: [Wit]
  } deriving (Show, Eq, Generic, Hashable, HasTypeReps)

instance HasHash [TxWits] where
  hash = Hash . H.hash

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

makeTxWits :: UTxO -> Tx -> TxWits
makeTxWits (UTxO utxo) tx = TxWits
  { body      = tx
  , witnesses = wits
  }
 where
  getKey txin =
    let
      TxOut (Addr (VKey o)) _ =
        fromMaybe
            (error "makeTxWits: Missing output for transaction input")
          $ Map.lookup txin utxo
    in KeyPair (SKey o) (VKey o)
  keys = getKey <$> inputs tx
  wits = makeWitness <$> keys <*> pure tx


--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

instance Goblin Bool Tx where
  tinker gen = do
    genX <- tinker ((\(Tx x _) -> x) <$> gen)
    genY <- tinker ((\(Tx _ y) -> y) <$> gen)
    pure (Tx <$> genX <*> genY)
  conjure = Tx <$> conjure <*> conjure
instance Goblin Bool TxId where
  tinker gen = do
    gen' <- tinker ((\(TxId w) -> w) <$> gen)
    pure (TxId <$> gen')
  conjure = TxId <$> conjure
instance Goblin Bool TxIn where
  tinker gen = do
    genX <- tinker ((\(TxIn x _) -> x) <$> gen)
    genY <- tinker ((\(TxIn _ y) -> y) <$> gen)
    pure (TxIn <$> genX <*> genY)
  conjure = TxIn <$> conjure <*> conjure
instance Goblin Bool TxOut where
  tinker gen = do
    genX <- tinker ((\(TxOut x _) -> x) <$> gen)
    genY <- tinker ((\(TxOut _ y) -> y) <$> gen)
    pure (TxOut <$> genX <*> genY)
  conjure = TxOut <$> conjure <*> conjure
instance Goblin Bool TxWits where
  tinker gen = do
    genX <- tinker ((\(TxWits x _) -> x) <$> gen)
    genY <- tinker ((\(TxWits _ y) -> y) <$> gen)
    pure (TxWits <$> genX <*> genY)
  conjure = TxWits <$> conjure <*> conjure
instance Goblin Bool Wit where
  tinker gen = do
    genX <- tinker ((\(Wit x _) -> x) <$> gen)
    genY <- tinker ((\(Wit _ y) -> y) <$> gen)
    pure (Wit <$> genX <*> genY)
  conjure = Wit <$> conjure <*> conjure
instance Goblin Bool (Sig Tx) where
  tinker gen = do
    genX <- tinker ((\(Sig x _) -> x) <$> gen)
    genY <- tinker ((\(Sig _ y) -> y) <$> gen)
    pure (Sig <$> genX <*> genY)
  conjure = Sig <$> conjure <*> conjure
instance Goblin Bool (Sig VKeyGenesis) where
  tinker gen = do
    genX <- tinker ((\(Sig x _) -> x) <$> gen)
    genY <- tinker ((\(Sig _ y) -> y) <$> gen)
    pure (Sig <$> genX <*> genY)
  conjure = Sig <$> conjure <*> conjure


--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

instance AddShrinks Tx where
  addShrinks = pure
instance AddShrinks TxId where
  addShrinks = pure
instance AddShrinks TxIn where
  addShrinks = pure
instance AddShrinks TxOut where
  addShrinks = pure
instance AddShrinks TxWits where
  addShrinks = pure
instance AddShrinks Wit where
  addShrinks = pure
instance AddShrinks (Sig Tx) where
  addShrinks = pure
instance AddShrinks (Sig VKeyGenesis) where
  addShrinks = pure


--------------------------------------------------------------------------------
-- ToExpr instances
--------------------------------------------------------------------------------

instance ToExpr (Sig VKeyGenesis)
instance ToExpr (Sig Tx)
instance ToExpr Tx
instance ToExpr TxId where
  toExpr (TxId (Hash i)) = App "TxId" [App "Hash" [toExpr i]]
instance ToExpr TxIn
instance ToExpr TxOut
instance ToExpr TxWits
instance ToExpr Wit
