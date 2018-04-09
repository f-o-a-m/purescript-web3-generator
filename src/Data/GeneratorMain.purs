module Data.GeneratorMain where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff)
import Data.CodeGen (generatePS)
import Node.Yargs.Applicative (yarg, flag, runY)
import Node.FS.Aff (FS)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp, example)

generatorMain :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e)  Unit
generatorMain =
    let setup = usage "$0 --abis <abis> --dest <dest> ./src --prefix <prefix> --module <module> --truffle <bool>"
             <> example "$0 --abis ./contract-abis --dest ./src --prefix eth_ --module MyApp.Contracts --truffle false " "Generate contract code from solidity compiler output in current directory with prefix `eth_` into the `src/MyApp/Contracts/` directory with module name `Myapp.Contracts.*`"
             <> defaultVersion
             <> defaultHelp
    in runY setup $ go <$> yarg "abis" [] Nothing (Right "Must specify abi source directory.") true
                       <*> yarg "dest" [] Nothing (Left "./src") false
                       <*> yarg "prefix" [] Nothing (Left "") false
                       <*> yarg "module" [] Nothing (Left "Contracts") false
                       <*> flag "truffle" [] (Just "Are the abi files truffle artifacts")
  where
    go abiDir destDir p modulePrefix tr = void <<< launchAff $ generatePS {jsonDir : abiDir, pursDir : destDir, truffle: tr, exprPrefix: p, modulePrefix: modulePrefix}
