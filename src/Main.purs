module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff)
import Data.Generator (generatePS)
import Node.Yargs.Applicative (yarg, runY)
import Node.FS.Aff (FS)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp)

main :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e)  Unit
main =
    let setup = usage "$0 --abis ./contract-abis" <> defaultVersion <> defaultHelp
    in runY setup $ go <$> yarg "abis" [] Nothing (Right "Must specify abi source directory.") true
  where
    go abiDir = void <<< launchAff $ generatePS {jsonDir : abiDir, pursDir : "./src"}
