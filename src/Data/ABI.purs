module Data.ABI where

import Prelude hiding (between)
import Control.Error.Util (hush, note)
import Control.Alternative ((<|>))
import Data.String (joinWith, fromCharArray)
import Data.Maybe (Maybe(..))
import Data.Array (some)
import Data.Int (fromString)
import Network.Ethereum.Web3.Types (HexString, sha3)
import Text.Parsing.Parser.String (string, char)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, try, choice)
import Text.Parsing.Parser.Token (digit)

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode ((.?))

--------------------------------------------------------------------------------
-- | Solidity Type Parsers
--------------------------------------------------------------------------------

data SolidityType =
    SolidityBool
  | SolidityUint
  | SolidityString
  | SolidityBytesN Int
  | SolidityBytesD
  | SolidityVector Int SolidityType
  | SolidityArray SolidityType

instance showSolidityType :: Show SolidityType where
  show s = case s of
    SolidityBool -> "bool"
    SolidityUint -> "uint256"
    SolidityString -> "string"
    SolidityBytesN n -> "bytes" <> "[" <> show n <> "]"
    SolidityBytesD -> "bytes"
    SolidityVector n a -> show a <> "[" <> show n <> "]"
    SolidityArray a -> show a <> "[]"

parseUint :: Parser String SolidityType
parseUint = string "uint" >>= \_ -> pure SolidityUint

parseBool :: Parser String SolidityType
parseBool = string "bool" >>= \_ -> pure SolidityBool

parseString :: Parser String SolidityType
parseString = string "string" >>= \_ -> pure SolidityString

numberParser :: Parser String Int
numberParser = do
  n <- fromCharArray <$> some digit
  case fromString n of
    Nothing -> fail $ "Couldn't parse as Natural : " <> n
    Just n' -> pure $ n'

parseBytesN :: Parser String SolidityType
parseBytesN = do
  _ <- string "bytes"
  n <- between (char '[') (char ']') numberParser
  pure $ SolidityBytesN n

parseBytesD :: Parser String SolidityType
parseBytesD = string "bytes" >>= \_ -> pure SolidityBytesD

parseBytes :: Parser String SolidityType
parseBytes = try parseBytesN <|> parseBytesD

solidityBasicTypeParser :: Parser String SolidityType
solidityBasicTypeParser =
    choice [ parseUint
           , parseBool
           , parseString
           , parseBytes
           ]

parseArray :: Parser String SolidityType
parseArray = do
  s <- solidityBasicTypeParser
  _ <- string "[]"
  pure $ SolidityArray s

parseVector :: Parser String SolidityType
parseVector = do
  s <- solidityBasicTypeParser
  n <- between (char '[') (char ']') numberParser
  pure $ SolidityVector n s


solidityTypeParser :: Parser String SolidityType
solidityTypeParser =
    choice [ parseArray
           , parseVector
           , solidityBasicTypeParser
           ]

parseSolidityType :: String -> Maybe SolidityType
parseSolidityType s = hush $ runParser s solidityTypeParser

instance decodeJsonSolidityType :: DecodeJson SolidityType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "type"
    note ("Failed to parse SolidityType: " <> t) $ parseSolidityType t

--------------------------------------------------------------------------------
-- | Solidity Function Parser
--------------------------------------------------------------------------------

data SolidityFunction =
  SolidityFunction { name :: String
                   , inputs :: Array SolidityType
                   , outputs :: Array SolidityType
                   , constant :: Boolean
                   }

instance decodeJsonSolidityFunction :: DecodeJson SolidityFunction where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .? "name"
    is <- obj .? "inputs"
    os <- obj .? "outputs"
    c <- obj .? "constant"
    pure $ SolidityFunction { name : nm
                            , inputs : is
                            , outputs : os
                            , constant : c
                            }



toSelector :: SolidityFunction -> HexString
toSelector (SolidityFunction f) =
  let args = map (\i -> show i) f.inputs
  in sha3 $ f.name <> "(" <> joinWith "," args <> ")"
