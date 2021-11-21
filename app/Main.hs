{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import Controller (AppContracts)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)
import Plutus.Contracts.PayToAddress (PayToAddressParams)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @AppContracts)


