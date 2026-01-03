{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Escrow where

import           PlutusLedgerApi.V2
import           PlutusTx
import           PlutusTx.Prelude
import           Prelude (Show)

data EscrowDatum = EscrowDatum
    { depositor   :: PubKeyHash
    , beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowRedeemer = Claim | Refund
PlutusTx.unstableMakeIsData ''EscrowRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
    case red of
        Claim  -> traceIfFalse "Not beneficiary" signedByBeneficiary
        Refund -> traceIfFalse "Too early" deadlinePassed &&
                  traceIfFalse "Not depositor" signedByDepositor
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByDepositor    = txSignedBy info (depositor dat)
    signedByBeneficiary  = txSignedBy info (beneficiary dat)

    deadlinePassed = contains (from (deadline dat)) (txInfoValidRange info)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Script
script = unValidatorScript validator

scriptHash :: ValidatorHash
scriptHash = validatorHash validator
