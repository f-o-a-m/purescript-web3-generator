module Test.Main where

import Prelude
import Test.Spec.Assertions (shouldEqual)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut.Parser (jsonParser)
import Test.Spec (Spec, describe, it)
import Data.Either (Either, either, isRight)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Data.CodeGen (generatePS, parseAbi)
import Data.AbiParser (Abi)

main :: Eff (RunnerEffects (fs :: FS, console :: CONSOLE )) Unit
main = run [consoleReporter] $ do
  simpleStorageParserSpec

simpleStorageParserSpec :: forall r . Spec (fs :: FS, console :: CONSOLE | r) Unit
simpleStorageParserSpec =
  describe "simple storage parser spec" do

    it "can parse the simple storage abi" do
      ejson <- jsonParser <$> readTextFile UTF8 "./abi-data/truffle/build/contracts/SimpleStorage.json"
      json <- either (throwError <<< error) pure ejson
      let (eabi :: Either String Abi) = parseAbi {truffle: true} json
      isRight eabi `shouldEqual` true

    it "can generate an encoding instance" do
      generatePS
        { jsonDir: "./abi-data/truffle/build/contracts"
        , pursDir: "./contracts"
        , truffle: true
        , exprPrefix: ""
        , modulePrefix: "ContractsTruffle"
        }
      generatePS
        { jsonDir: "./abi-data/abis"
        , pursDir: "./contracts"
        , truffle: false
        , exprPrefix: ""
        , modulePrefix: "Contracts"
        }
