{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.Contracts.PayToAddress(
      payToAddress
    , PayToAddressParams(..)
    , PayToAddressSchema
    , buildParamsJson
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Schema (ToSchema)

import Ledger (Value, getCardanoTxId)
import Ledger.Constraints
import Plutus.V1.Ledger.Address (Address, pubKeyHashAddress)
import Plutus.V1.Ledger.Api (PubKeyHash, getPubKeyHash)
import Plutus.Contract
import Wallet.Emulator.Types (Wallet, walletPubKeyHash)
import Wallet.Emulator.Wallet
import Ledger.Ada
import Data.Either
import Data.Text as T
import Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BSL
import Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import Cardano.Api (Address,  AsType( AsShelleyAddress ), ShelleyAddr, deserialiseFromBech32, HasTypeProxy)
import Cardano.Api.Shelley (Address (ShelleyAddress))
import Cardano.Ledger.Shelley.API (Credential (KeyHashObj, ScriptHashObj))

data PayToAddressParams =
    PayToAddressParams
        { amount :: Ledger.Value
        , payee :: PubKeyHash
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type PayToAddressSchema = Endpoint "payAddress" PayToAddressParams

payToAddress :: Promise () PayToAddressSchema ContractError ()
payToAddress = endpoint @"payAddress" $ \PayToAddressParams{amount, payee} -> do
  txid <- submitTx (mustPayToPubKey payee amount)
  awaitTxConfirmed (getCardanoTxId txid)

addrToPkh :: Cardano.Api.Address ShelleyAddr -> PubKeyHash
--addrToPkh (ShelleyAddress net (ScriptHashObj sh) sr) = transKeyHash sh
addrToPkh (ShelleyAddress net (KeyHashObj kh) sr) = transKeyHash kh

buildParams :: Integer -> Cardano.Api.Address ShelleyAddr -> PayToAddressParams
buildParams lovelaces address = PayToAddressParams { amount = lovelaceValueOf lovelaces, payee = (addrToPkh address) }

buildParamsJson :: Integer -> Text -> IO ()
buildParamsJson lovelaces bech32Address = BSL.putStrLn $ A.encode $ (buildParams lovelaces (getRightAddress (getEitherAddress bech32Address)))
                                          where getEitherAddress text = deserialiseFromBech32 AsShelleyAddress text
                                                getRightAddress (Right addr) = addr