module Data.GeneratorMain where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff)
import Data.Generator (generatePS)
import Node.Yargs.Applicative (yarg, flag, runY)
import Node.FS.Aff (FS)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp, example)

generatorMain :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e)  Unit
generatorMain =
    let setup = usage "$0 --abis ./contract-abis --dest destination -" 
             <> example "$0 --abis ./contract-abis" "Generate contracts in current directory"
             <> defaultVersion 
             <> defaultHelp
    in runY setup $ go <$> yarg "abis" [] Nothing (Right "Must specify abi source directory.") true
                       <*> yarg "dest" [] Nothing (Left "./src") false
                       <*> yarg "prefix" [] Nothing (Left "") false
                       <*> flag "truffle" [] (Just "Are the abi files truffle artifacts")
  where
    go abiDir destDir p tr = void <<< launchAff $ generatePS {jsonDir : abiDir, pursDir : destDir, truffle: tr, prefix: p}
