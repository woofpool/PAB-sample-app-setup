{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Controller (
    AppContracts(..)
    , handlers
    ) where

import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Prettyprinter

import Data.OpenApi.Schema qualified as OpenApi
import Playground.Types (FunctionSchema)
import Plutus.Contracts.PayToWallet qualified as Contracts.PayToWallet
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Schema (FormSchema)

data AppContracts = PayToWallet
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty AppContracts where
    pretty = viaShow

instance HasPSTypes AppContracts where
    psTypes = []

instance HasDefinitions AppContracts where
    getDefinitions = [ PayToWallet ]
    getContract = getAppContracts
    getSchema = getAppContractsSchema

getAppContractsSchema :: AppContracts -> [FunctionSchema FormSchema]
getAppContractsSchema = \case
    PayToWallet      -> Builtin.endpointsToSchemas @Contracts.PayToWallet.PayToWalletSchema

getAppContracts :: AppContracts -> SomeBuiltin
getAppContracts = \case
    PayToWallet      -> SomeBuiltin Contracts.PayToWallet.payToWallet

handlers :: SimulatorEffectHandlers (Builtin AppContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
