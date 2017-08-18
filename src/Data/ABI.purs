module Data.ABI where

import Prelude hiding (between)
import Control.Alternative ((<|>))
import Data.String (joinWith, fromCharArray, toUpper, take, drop)
import Data.Maybe (Maybe(..))
import Data.Array (some)
import Data.Int (fromString)
import Data.Either (Either(..))
import Network.Ethereum.Web3.Types (HexString, sha3)
import Text.Parsing.Parser.String (string, char)
import Data.EitherR (fmapL)
import Text.Parsing.Parser (Parser, parseErrorMessage, fail, runParser)
import Text.Parsing.Parser.Combinators (between, try, choice)
import Text.Parsing.Parser.Token (digit)
import Data.Generic (class Generic, gShow)

import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode ((.?))

class Format a where
  format :: a -> String

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

derive instance genericSolidityType :: Generic SolidityType

instance showSolidityType :: Show SolidityType where
  show = gShow

instance formatSolidityType :: Format SolidityType where
  format s = case s of
    SolidityBool -> "bool"
    SolidityUint -> "uint256"
    SolidityString -> "string"
    SolidityBytesN n -> "bytes" <> "[" <> show n <> "]"
    SolidityBytesD -> "bytes"
    SolidityVector n a -> format a <> "[" <> show n <> "]"
    SolidityArray a -> format a <> "[]"

parseUint :: Parser String SolidityType
parseUint = string "uint256" >>= \_ -> pure SolidityUint

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
    choice [ try parseUint
           , try parseBool
           , try parseString
           , try parseBytes
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
    choice [ try parseArray
           , try parseVector
           , try solidityBasicTypeParser
           ]

parseSolidityType :: String -> Either String SolidityType
parseSolidityType s = fmapL parseErrorMessage $ runParser s solidityTypeParser

instance decodeJsonSolidityType :: DecodeJson SolidityType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "type"
    case parseSolidityType t of
      Left err -> Left $ "Failed to parse SolidityType " <> t <> " : "  <> err
      Right typ -> Right typ

--------------------------------------------------------------------------------
-- | Solidity Function Parser
--------------------------------------------------------------------------------

data SolidityFunction =
  SolidityFunction { name :: String
                   , inputs :: Array SolidityType
                   , outputs :: Array SolidityType
                   , constant :: Boolean
                   }

derive instance genericSolidityFunction :: Generic SolidityFunction

instance showSolidityFunction :: Show SolidityFunction where
  show = gShow

toUpperHead :: String -> String
toUpperHead s =
  let h = toUpper $ take 1 s
      rest = drop 1 s
  in h <> rest

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
  let args = map (\i -> format i) f.inputs
  in sha3 $ f.name <> "(" <> joinWith "," args <> ")"

--------------------------------------------------------------------------------
-- | Solidity Constructor Parser
--------------------------------------------------------------------------------

data SolidityConstructor =
  SolidityConstructor { inputs :: Array SolidityType
                      }

derive instance genericSolidityConstructor :: Generic SolidityConstructor

instance showSolidityConstructor :: Show SolidityConstructor where
  show = gShow

instance decodeJsonSolidityConstructor :: DecodeJson SolidityConstructor where
  decodeJson json = do
    obj <- decodeJson json
    is <- obj .? "inputs"
    pure $ SolidityConstructor { inputs : is
                               }

--------------------------------------------------------------------------------
-- | Solidity Events Parser
--------------------------------------------------------------------------------

data IndexedSolidityValue =
  IndexedSolidityValue { type :: SolidityType
                       , name :: String
                       , indexed :: Boolean
                       }

derive instance genericSolidityIndexedValue :: Generic IndexedSolidityValue

instance showSolidityIndexedValue :: Show IndexedSolidityValue where
  show = gShow

instance decodeJsonIndexedSolidityValue :: DecodeJson IndexedSolidityValue where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .? "name"
    ts <- obj .? "type"
    t <- parseSolidityType ts
    ixed <- obj .? "indexed"
    pure $ IndexedSolidityValue { name : nm
                                , type : t
                                , indexed : ixed
                                }

data SolidityEvent =
  SolidityEvent { name :: String
                , anonymous :: Boolean
                , inputs :: Array IndexedSolidityValue
                }

derive instance genericSolidityEvent :: Generic SolidityEvent

instance showSolidityEvent :: Show SolidityEvent where
  show = gShow

instance decodeJsonSolidityEvent :: DecodeJson SolidityEvent where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .? "name"
    is <- obj .? "inputs"
    a <- obj .? "anonymous"
    pure $ SolidityEvent { name : nm
                         , inputs : is
                         , anonymous : a
                         }

--------------------------------------------------------------------------------
-- | ABI
--------------------------------------------------------------------------------

data AbiType =
    AbiFunction SolidityFunction
  | AbiConstructor SolidityConstructor
  | AbiEvent SolidityEvent

derive instance genericAbiType :: Generic AbiType

instance showAbiType :: Show AbiType where
  show = gShow

instance decodeJsonAbiType :: DecodeJson AbiType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "type"
    let json' = fromObject obj
    case t of
      "function" -> AbiFunction <$> decodeJson json'
      "constructor" -> AbiConstructor <$> decodeJson json'
      "event" -> AbiEvent <$> decodeJson json'
      _ -> Left $ "Unkown abi type: " <> t


newtype Abi = Abi (Array AbiType)

derive newtype instance decodeJsonAbi :: DecodeJson Abi

derive newtype instance showAbi :: Show Abi

