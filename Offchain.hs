{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Offchain where

import           Control.Monad        (void)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           Ledger
import           Ledger.Constraints   as Constraints
import           PlutusLedgerApi.V2
import           Escrow
import           PlutusTx.Prelude
import           Prelude (Show)

data DepositParams = DepositParams
    { dpBeneficiary :: PubKeyHash
    , dpDeadline    :: POSIXTime
    , dpAmount      :: Integer
    } deriving (Show, Generic)

type EscrowSchema =
        Endpoint "deposit" DepositParams
     .\/ Endpoint "claim" ()
     .\/ Endpoint "refund" ()

depositFunds :: DepositParams -> Contract () EscrowSchema Text ()
depositFunds p = do
    self <- ownPubKeyHash
    let dat = EscrowDatum self (dpBeneficiary p) (dpDeadline p)
        tx  = Constraints.mustPayToOtherScript
              scriptHash
              (Datum $ toBuiltinData dat)
              (lovelaceValueOf $ dpAmount p)

    void (submitTxConstraints (Constraints.otherScript validator) tx)

claimFunds :: () -> Contract () EscrowSchema Text ()
claimFunds _ = do
    utxos <- utxosAt (scriptHashAddress scriptHash)
    void $ submitTxConstraintsSpending validator utxos (Redeemer $ toBuiltinData Claim)

refundFunds :: () -> Contract () EscrowSchema Text ()
refundFunds _ = do
    utxos <- utxosAt (scriptHashAddress scriptHash)
    void $ submitTxConstraintsSpending validator utxos (Redeemer $ toBuiltinData Refund)

endpoints :: Contract () EscrowSchema Text ()
endpoints =
    awaitPromise (deposit' `select` claim' `select` refund') >> endpoints
  where
    deposit' = endpoint @"deposit" depositFunds
    claim'   = endpoint @"claim" claimFunds
    refund'  = endpoint @"refund" refundFunds
