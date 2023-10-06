module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.AbiParser (AbiWithErrors)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (null)
import Data.CodeGen (generatePS, parseAbi)
import Data.Either (Either, either, isRight)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_
  $ runSpec [ consoleReporter ]
  $ do
      simpleStorageParserSpec

simpleStorageParserSpec :: Spec Unit
simpleStorageParserSpec =
  describe "simple storage parser spec" do

    it "can parse the simple storage abi" do
      ejson <- jsonParser <$> readTextFile UTF8 "./abi-data/truffle/build/contracts/SimpleStorage.json"
      json <- either (throwError <<< error) pure ejson
      let (eabi :: Either String AbiWithErrors) = parseAbi { truffle: true } json
      -- Note: we could check if there are errors in `AbiWithErrors`,
      -- but it will be checked as part of `generatePS` tests later.
      isRight eabi `shouldEqual` true

    it "can generate an encoding instance" do
      let
        shouldHaveNoErrors errs = unless (null errs) do
          fail $ "Got errors during generation: " <> show errs
      shouldHaveNoErrors =<< generatePS
        { jsonDir: "./abi-data/truffle/build/contracts"
        , pursDir: "./contracts"
        , truffle: true
        , exprPrefix: ""
        , modulePrefix: "ContractsTruffle"
        }

      shouldHaveNoErrors =<< generatePS
        { jsonDir: "./abi-data/abis"
        , pursDir: "./contracts"
        , truffle: false
        , exprPrefix: ""
        , modulePrefix: "Contracts"
        }

