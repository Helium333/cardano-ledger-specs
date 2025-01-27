{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UTXO transition system with witnessing

module Cardano.Ledger.Spec.STS.UTXOW where

import qualified Data.Map as Map

import           Control.State.Transition (Embed, Environment, IRC (IRC), PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext, trans,
                     transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (Addr (Addr), KeyPair (KeyPair), VKey, keyPair, mkAddr, owner, sign,
                     verify)
import qualified Ledger.Update.Generators as UpdateGen
import           Ledger.UTxO (Tx, TxIn, TxOut (TxOut), TxWits (TxWits), UTxO (UTxO), Wit (Wit),
                     body, fromTxOuts, inputs, pcMinFee)
import qualified Ledger.UTxO.Generators as UTxOGen

import           Cardano.Ledger.Spec.STS.UTXO

data UTXOW

instance STS UTXOW where

  type Environment UTXOW = UTxOEnv
  type State UTXOW = UTxOState
  type Signal UTXOW = TxWits
  data PredicateFailure UTXOW
    = UtxoFailure (PredicateFailure UTXO)
    | InsufficientWitnesses
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @UTXO $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxoSt@UTxOState {utxo}, tw) <- judgmentContext
        witnessed tw utxo ?! InsufficientWitnesses
        utxoSt' <- trans @UTXO $ TRC (env, utxoSt, body tw)
        return utxoSt'
    ]

-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> key == pay
  _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> UTxO -> Bool
witnessed (TxWits tx wits) utxo =
  length wits == length ins && all (isWitness tx utxo) (zip ins wits)
 where
  ins = inputs tx
  isWitness tx' unspent (input, Wit key sig) =
    verify key tx' sig && authTxin key input unspent


instance Embed UTXO UTXOW where
  wrapFailed = UtxoFailure

-- | Constant list of addresses intended to be used in the generators.
traceAddrs :: [Addr]
traceAddrs = mkAddr <$> [0 .. 10]

instance HasTrace UTXOW where
  envGen _
    = UTxOEnv <$> genUTxO <*> UpdateGen.pparamsGen
    where
      genUTxO = do
        txOuts <- UTxOGen.genInitialTxOuts traceAddrs
        -- All the outputs in the initial UTxO need to refer to some
        -- transaction id. Since there are no transactions where these outputs
        -- come from we use the hash of the address as transaction id.
        pure $ fromTxOuts txOuts

  sigGen _ UTxOEnv { pps } st = do
    tx <- UTxOGen.genTxFromUTxO traceAddrs (pcMinFee pps) (utxo st)
    let wits = witnessForTxIn tx (utxo st) <$> inputs tx
    pure $ TxWits tx wits

witnessForTxIn :: Tx -> UTxO -> TxIn -> Wit
witnessForTxIn tx (UTxO utxo) txin =
  case Map.lookup txin utxo of
    Just (TxOut (Addr pay) _) ->
      witnessForTx (keyPair $ owner pay) tx
    Nothing                   ->
      error "The generators must ensure that we are spending unspent inputs"

witnessForTx :: KeyPair -> Tx -> Wit
witnessForTx (KeyPair sk vk) tx = Wit vk (sign sk tx)
