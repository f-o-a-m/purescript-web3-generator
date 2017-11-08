module Data.AbiParser where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode ((.?))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Foldable (foldMap)
import Data.Generic (class Generic, gShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCharArray)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.Combinators (lookAhead, choice, many1Till, optionMaybe, many1)
import Text.Parsing.StringParser.String (anyDigit, string, char, eof)

--------------------------------------------------------------------------------

class Format a where
  format :: a -> String

--------------------------------------------------------------------------------
-- | Solidity Type Parsers
--------------------------------------------------------------------------------

data SolidityType =
    SolidityBool
  | SolidityAddress
  | SolidityUint Int
  | SolidityInt Int
  | SolidityString
  | SolidityBytesN Int
  | SolidityBytesD
  | SolidityVector (Array Int) SolidityType
  | SolidityArray SolidityType

derive instance genericSolidityType :: Generic SolidityType

instance showSolidityType :: Show SolidityType where
  show = gShow

instance formatSolidityType :: Format SolidityType where
  format s = case s of
    SolidityBool -> "bool"
    SolidityAddress -> "address"
    SolidityUint n -> "uint" <> show n
    SolidityInt n -> "int" <> show n
    SolidityString -> "string"
    SolidityBytesN n -> "bytes" <> show n
    SolidityBytesD -> "bytes"
    SolidityVector ns a -> format a <> foldMap (\n -> "[" <> show n <> "]") ns
    SolidityArray a -> format a <> "[]"

parseUint :: Parser SolidityType
parseUint = do
  _ <- string "uint"
  n <- numberParser
  pure $ SolidityUint n

parseInt :: Parser SolidityType
parseInt = do
  _ <- string "int"
  n <- numberParser
  pure $ SolidityInt n

parseBool :: Parser SolidityType
parseBool = string "bool" >>= \_ -> pure SolidityBool

parseString :: Parser SolidityType
parseString = string "string" >>= \_ -> pure SolidityString

numberParser :: Parser Int
numberParser = do
  n <- fromCharArray <<< fromFoldable <$> many1 anyDigit
  case fromString n of
    Nothing -> fail $ "Couldn't parse as Natural : " <> n
    Just n' -> pure $ n'

parseBytes :: Parser SolidityType
parseBytes = do
  _ <- string "bytes"
  mn <- optionMaybe numberParser
  pure $ maybe SolidityBytesD SolidityBytesN  mn


parseAddress :: Parser SolidityType
parseAddress = string "address" >>= \_ -> pure SolidityAddress

solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
    choice [ parseUint
           , parseInt
           , parseAddress
           , parseBool
           , parseString
           , parseBytes
           , parseAddress
           ]

parseVector :: Parser SolidityType
parseVector = do
    s <- solidityBasicTypeParser
    ns <- many1Till lengthParser ((lookAhead $ void (string "[]")) <|> eof)
    pure $ SolidityVector (fromFoldable $ ns) s
  where
    lengthParser = do
          _ <- char '['
          n <- numberParser
          _ <- char ']'
          pure n

parseArray :: Parser SolidityType
parseArray = do
  s <- (try $ parseVector <* string "[]") <|> (solidityBasicTypeParser <* string "[]")
  pure $ SolidityArray s


solidityTypeParser :: Parser SolidityType
solidityTypeParser =
    choice [ try parseArray
           , try parseVector
           , solidityBasicTypeParser
           ]

parseSolidityType :: String -> Either String SolidityType
parseSolidityType s = fmapL show $ runParser solidityTypeParser s

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
                   , payable :: Boolean
                   }

derive instance genericSolidityFunction :: Generic SolidityFunction

instance showSolidityFunction :: Show SolidityFunction where
  show = gShow


instance decodeJsonSolidityFunction :: DecodeJson SolidityFunction where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .? "name"
    is <- obj .? "inputs"
    os <- obj .? "outputs"
    c <- obj .? "constant"
    p <- obj .? "payable"
    pure $ SolidityFunction { name : nm
                            , inputs : is
                            , outputs : os
                            , constant : c
                            , payable: p
                            }

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

instance formatIndexedSolidityValue :: Format IndexedSolidityValue where
  format (IndexedSolidityValue v) = format v.type

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

data SolidityFallback = SolidityFallback

derive instance genericSolidityFallback :: Generic SolidityFallback

instance showSolidityFallback :: Show SolidityFallback where
  show = gShow

instance decodeJsonSolidityFallback :: DecodeJson SolidityFallback where
  decodeJson json = do
    pure $ SolidityFallback

--------------------------------------------------------------------------------
-- | ABI
--------------------------------------------------------------------------------

data AbiType =
    AbiFunction SolidityFunction
  | AbiConstructor SolidityConstructor
  | AbiEvent SolidityEvent
  | AbiFallback SolidityFallback

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
      "fallback" -> AbiFallback <$> decodeJson json'
      _ -> Left $ "Unkown abi type: " <> t


newtype Abi = Abi (Array AbiType)

derive newtype instance decodeJsonAbi :: DecodeJson Abi

derive newtype instance showAbi :: Show Abi
