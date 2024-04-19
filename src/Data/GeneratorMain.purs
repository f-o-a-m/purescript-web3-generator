module Data.GeneratorMain where

import Prelude

import Data.Array (fold)
import Data.Array (null) as A
import Data.CodeGen (GeneratorOptions, generatePS)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process (exit')
import Options.Applicative (Parser, ParserInfo, boolean, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, showDefault, strOption, value, (<**>))

data Args = Args GeneratorOptions

argsParser :: Parser Args
argsParser = ado
  jsonDir <-
    ( strOption $ fold
        [ long "abis"
        , metavar "ABIS"
        , showDefault
        , help "The abi source directory."
        ]
    )
  pursDir <-
    ( strOption $ fold
        [ long "dest"
        , metavar "DEST"
        , value "./src"
        , showDefault
        , help "The destination directory for purescript code."
        ]
    )
  exprPrefix <-
    ( strOption $ fold
        [ long "prefix"
        , metavar "PREFIX"
        , value ""
        , help "The expression prefix for the generated purescript code (used to get around solidity identifiers that generate invalide Purescript)."
        ]
    )
  modulePrefix <-
    ( strOption $ fold
        [ long "module"
        , metavar "MODULE"
        , value "Contracts"
        , help "The module name prefix for the generated purescript code."
        ]
    )
  truffle <-
    ( option boolean $ fold
        [ long "truffle"
        , metavar "TRUFFLE"
        , help "Are the abi files truffle artifacts"
        , value false
        ]
    )
  in Args { jsonDir, pursDir, truffle, exprPrefix, modulePrefix }

generatorMain :: Effect Unit
generatorMain = launchAff_ do
  (Args args) <- liftEffect $ execParser opts
  errs <- generatePS args
  liftEffect <<< exit' $
    if (A.null errs) then 0 else 1
  where
  opts :: ParserInfo Args
  opts = info (argsParser <**> helper)
    ( fullDesc
        <> progDesc "Purescript Web3 Generator"
        <> header "ps-web3-generator - generate Purescript bindings to your solidity contracts"
    )
