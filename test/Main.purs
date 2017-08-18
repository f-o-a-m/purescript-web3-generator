module Test.Main where

import Prelude
-- import Test.Spec.Assertions (shouldEqual)
-- import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either, either)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Data.ABI (Abi)

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | e) Unit
main = do
  ejson <- jsonParser <$> readTextFile UTF8 "./abi-data/SimpleStorage.json"
  json <- either throw pure ejson
  logShow $ json
  let (eabi :: Either String Abi) = decodeJson json
  logShow $ eabi
