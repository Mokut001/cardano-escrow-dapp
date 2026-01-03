{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Servant
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Data.Aeson
import           GHC.Generics

data DepositReq = DepositReq
  { beneficiary :: String
  , deadline    :: Integer
  , amount      :: Integer
  } deriving (Generic, Show, FromJSON)

type API =
         "deposit" :> ReqBody '[JSON] DepositReq :> Post '[JSON] String
    :<|> "claim"   :> Post '[JSON] String
    :<|> "refund"  :> Post '[JSON] String

server :: Server API
server =
    handleDeposit :<|> handleClaim :<|> handleRefund
  where
    handleDeposit _ = pure "Deposit submitted."
    handleClaim     = pure "Claim submitted."
    handleRefund    = pure "Refund submitted."

api :: Proxy API
api = Proxy

main :: IO ()
main = run 8080 (serve api server)
