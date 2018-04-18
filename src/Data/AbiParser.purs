module Data.AbiParser where

import Prelude

import Control.Alternative ((<|>))
import Control.Error.Util (note)
import Data.Argonaut as A
import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode ((.?))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array (fromFoldable, uncons, (:))
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Foldable (foldMap)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.Record.Extra (showRecord)
import Data.String (fromCharArray)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, parseBigNumber, toString)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.Combinators (lookAhead, choice, manyTill, optionMaybe, many1)
import Text.Parsing.StringParser.String (anyDigit, string, char, eof)

--------------------------------------------------------------------------------

class Format a where
  format :: a -> String

--------------------------------------------------------------------------------
-- | Solidity Type Parsers
--------------------------------------------------------------------------------

data SolidityType
  = SolidityBool
  | SolidityAddress
  | SolidityUint BigNumber
  | SolidityInt BigNumber
  | SolidityString
  | SolidityBytesN BigNumber
  | SolidityBytesD
  | SolidityVector (NonEmptyList BigNumber) SolidityType
  | SolidityArray SolidityType


instance showSolidityType :: Show SolidityType where
  show = case _ of
    SolidityBool -> "(SolidityBool)"
    SolidityAddress -> "(SolidityAddress)"
    SolidityUint n -> "(SolidityUint "<> show n <> ")"
    SolidityInt n -> "(SolidityInt "<> show n <> ")"
    SolidityString -> "(SolidityString)"
    SolidityBytesN n -> "(SolidityBytesN "<> show n <> ")"
    SolidityBytesD -> "(SolidityBytesD)"
    SolidityVector ns st -> "(SolidityVector "<> show ns <> " " <> show st <> ")"
    SolidityArray st -> "(SolidityArray "<> show st <> ")"

instance formatSolidityType :: Format SolidityType where
  format s = case s of
    SolidityBool -> "bool"
    SolidityAddress -> "address"
    SolidityUint n -> "uint" <> toString decimal n
    SolidityInt n -> "int" <> toString decimal n
    SolidityString -> "string"
    SolidityBytesN n -> "bytes" <> toString decimal n
    SolidityBytesD -> "bytes"
    SolidityVector ns a -> format a <> foldMap (\n -> "[" <> toString decimal n <> "]") ns
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

numberParser :: Parser BigNumber
numberParser = do
  n <- fromCharArray <<< fromFoldable <$> many1 anyDigit
  case parseBigNumber decimal n of
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
    n <- lengthParser
    ns <- manyTill lengthParser ((lookAhead $ void (string "[]")) <|> eof)
    pure $ SolidityVector (NonEmptyList $ n :| ns) s
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

newtype FunctionInput =
  FunctionInput { name :: String
                , type :: SolidityType
                }

instance showFunctionInput :: Show FunctionInput where
  show (FunctionInput r)= "(FunctionInput " <> showRecord r <> ")"

instance formatInput :: Format FunctionInput where
  format (FunctionInput fi) = format fi.type

instance decodeFunctionInput :: DecodeJson FunctionInput where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .? "name"
    t <- obj .? "type"
    case parseSolidityType t of
      Left err -> Left $ "Failed to parse SolidityType " <> t <> " : "  <> err
      Right typ -> Right $ FunctionInput {type: typ, name: n}

data SolidityFunction =
  SolidityFunction { name :: String
                   , inputs :: Array FunctionInput
                   , outputs :: Array SolidityType
                   , constant :: Boolean
                   , payable :: Boolean
                   , isConstructor :: Boolean
                   }
instance showSolidityFunction :: Show SolidityFunction where
  show (SolidityFunction r) = "(SolidityFunction " <> showRecord r <> ")"

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
                            , isConstructor: false
                            }

--------------------------------------------------------------------------------
-- | Solidity Constructor Parser
--------------------------------------------------------------------------------

data SolidityConstructor =
  SolidityConstructor { inputs :: Array FunctionInput
                      }

instance showSolidityConstructor :: Show SolidityConstructor where
  show (SolidityConstructor r)= "(SolidityConstructor " <> showRecord r <> ")"

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

instance showSolidityIndexedValue :: Show IndexedSolidityValue where
  show (IndexedSolidityValue r)= "(IndexedSolidityValue " <> showRecord r <> ")"

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

instance showSolidityEvent :: Show SolidityEvent where
  show (SolidityEvent r) = "(SolidityEvent " <> showRecord r <> ")"

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

instance showSolidityFallback :: Show SolidityFallback where
  show _ = "(SolidityFallback)"

instance decodeJsonSolidityFallback :: DecodeJson SolidityFallback where
  decodeJson json = do
    pure $ SolidityFallback

--------------------------------------------------------------------------------
-- | ABI
--------------------------------------------------------------------------------

data AbiType
  = AbiFunction SolidityFunction
  | AbiConstructor SolidityConstructor
  | AbiEvent SolidityEvent
  | AbiFallback SolidityFallback

instance showAbiType :: Show AbiType where
  show = case _ of
    AbiFunction r -> "(AbiFunction " <> show r <> ")"
    AbiConstructor r -> "(AbiConstructor " <> show r <> ")"
    AbiEvent r -> "(AbiEvent " <> show r <> ")"
    AbiFallback r -> "(AbiFallback " <> show r <> ")"

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

instance decodeJsonAbi :: DecodeJson Abi where
  decodeJson json = do
    arr <- note "Failed to decode ABI as Array type." $ A.toArray json
    pure $ Abi $ catEithers $ map decodeJson arr
    where
      catEithers :: forall a b. Array (Either a b) -> Array b
      catEithers es = case uncons es of
        Just {head, tail} -> case head of
          Right b -> b : catEithers tail
          Left _ -> catEithers tail
        Nothing -> []


derive newtype instance showAbi :: Show Abi
