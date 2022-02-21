module Web3Generator.AbiParser where

import Prelude

import Control.Alternative ((<|>))
import Control.Error.Util (note)
import Data.Argonaut (Json)
import Data.Argonaut as A
import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (fromFoldable, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (all, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.TacitString as TacitString
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (anyDigit, string, char, eof)
import Text.Parsing.StringParser.Combinators (choice, lookAhead, manyTill, many1, optionMaybe)

--------------------------------------------------------------------------------

class Format a where
  format :: a -> String

--------------------------------------------------------------------------------
-- | Solidity Type Parsers
--------------------------------------------------------------------------------

data SolidityType
  = SolidityBool
  | SolidityAddress
  | SolidityUint Int
  | SolidityInt Int
  | SolidityString
  | SolidityBytesN Int
  -- dynamically sized array
  | SolidityBytesD
  -- Example:
  -- `SolidityVector [1,2] SolidityBool`
  -- equal to `bool[1][2]` - 1 boolean inside 1-elem array, which is inside of 2-elem array
  -- printed as `Vector D2 (Vector D1 Boolean)`
  | SolidityVector (NonEmptyList Int) SolidityType
  -- Example:
  -- `SolidityArray SolidityBool`
  -- equal to `bool[]` - dinamically sized array of booleans
  -- printed as `Array Boolean`
  | SolidityArray SolidityType

derive instance genericSolidityType :: Generic SolidityType _

instance showSolidityType :: Show SolidityType where
  show x = genericShow x

instance eqSolidityType :: Eq SolidityType where
  eq x = genericEq x

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
  n <- parseDigits >>= asInt
  pure $ SolidityUint n

parseInt :: Parser SolidityType
parseInt = do
  _ <- string "int"
  n <- parseDigits >>= asInt
  pure $ SolidityInt n

parseBool :: Parser SolidityType
parseBool = string "bool" >>= \_ -> pure SolidityBool

parseString :: Parser SolidityType
parseString = string "string" >>= \_ -> pure SolidityString

parseDigits :: Parser String
parseDigits = fromCharArray <<< fromFoldable <$> many1 (try anyDigit)

asInt :: String -> Parser Int
asInt n = case fromString n of
  Nothing -> do
    fail $ "Couldn't parse as Int : " <> n
  Just n' -> pure $ n'

parseBytesNAndD :: Parser SolidityType
parseBytesNAndD = do
  _ <- string "bytes"
  mns <- optionMaybe parseDigits
  case mns of
    Nothing -> pure SolidityBytesD
    Just ns -> do
      n <- asInt ns
      pure $ SolidityBytesN n

parseAddress :: Parser SolidityType
parseAddress = string "address" >>= \_ -> pure SolidityAddress

-- everything except vector and array
solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
  choice
    [ parseUint
    , parseInt
    , parseAddress
    , parseBool
    , parseString
    , parseBytesNAndD
    ]

vectorDimentionsParser :: Parser (List Int)
vectorDimentionsParser = manyTill
  (char '[' *> (parseDigits >>= asInt) <* char ']')
  (lookAhead $ void (string "[]") <|> eof)

solidityTypeParser :: Parser SolidityType
solidityTypeParser = do
  t <- solidityBasicTypeParser
  mbVectorDims <- vectorDimentionsParser
  let
    t' = case mbVectorDims of
      Nil -> t
      Cons n ns -> SolidityVector (NonEmptyList $ n :| ns) t
  (SolidityArray t' <$ string "[]") <|> pure t'

parseSolidityType :: String -> Either JsonDecodeError SolidityType
parseSolidityType s = parseSolidityType' s # lmap \err -> Named err $ UnexpectedValue (encodeJson s)

parseSolidityType' :: String -> Either String SolidityType
parseSolidityType' s = runParser (solidityTypeParser <* eof) s # lmap \err ->
  "Failed to parse SolidityType " <> show s <> " with error: " <> show err

instance decodeJsonSolidityType :: DecodeJson SolidityType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    parseSolidityType t

--------------------------------------------------------------------------------
-- | Solidity Function Parser
--------------------------------------------------------------------------------

newtype FunctionInput =
  FunctionInput
    { name :: String
    , type :: SolidityType
    }

derive instance genericFunctionInput :: Generic FunctionInput _

instance showFunctionInput :: Show FunctionInput where
  show = genericShow

instance formatInput :: Format FunctionInput where
  format (FunctionInput fi) = format fi.type

instance decodeFunctionInput :: DecodeJson FunctionInput where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .: "name"
    t <- obj .: "type"
    typ <- parseSolidityType t
    pure $ FunctionInput { type: typ, name: n }

newtype SolidityFunction =
  SolidityFunction
    { name :: String
    , inputs :: Array FunctionInput
    , outputs :: Array SolidityType
    , constant :: Boolean
    , payable :: Boolean
    , isConstructor :: Boolean -- TODO: remove
    , isUnCurried :: Boolean -- TODO: remove
    }

derive instance genericSolidityFunction :: Generic SolidityFunction _

instance showSolidityFunction :: Show SolidityFunction where
  show = genericShow

instance decodeJsonSolidityFunction :: DecodeJson SolidityFunction where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .: "name"
    is <- obj .: "inputs"
    os <- obj .: "outputs"
    let
      parseStateMutability = do
        sm <- obj .: "stateMutability"
        str <- decodeJson sm
        case str of
          "pure" -> pure { constant: true, payable: false }
          "view" -> pure { constant: true, payable: false }
          "payable" -> pure { constant: false, payable: true }
          "nonpayable" -> pure { constant: false, payable: false }
          _ -> Left $ Named "Expecting \"stateMutabiltiy\" to be one of \"pure\", \"view\", \"payable\", or \"nonpayable\"" $ UnexpectedValue sm
    let
      parseConstantPayableFields = do
        constant <- obj .: "constant"
        payable <- obj .: "payable"
        pure { constant, payable }
      fallback = Left $ Named "Expected a \"stateMutability\" field or a combination of \"constant\" and \"payable\" fields" $ UnexpectedValue json
    cp <- parseStateMutability <|> parseConstantPayableFields <|> fallback
    pure $ SolidityFunction
      { name: nm
      , inputs: is
      , outputs: os
      , constant: cp.constant
      , payable: cp.payable
      , isConstructor: false
      , isUnCurried: all (\(FunctionInput fi) -> fi.name /= "") is
          && not (null is)
      }

--------------------------------------------------------------------------------
-- | Solidity Constructor Parser
--------------------------------------------------------------------------------

newtype SolidityConstructor =
  SolidityConstructor
    { inputs :: Array FunctionInput
    -- curried: `constructor()` OR `constructor(string, uint256)` OR `constructor(foofield string, uint256)`
    -- uncurried: `constructor(foofield string, foofield2 uint256)`
    , isUnCurried :: Boolean
    }

derive instance genericSolidityConstructor :: Generic SolidityConstructor _

instance showSolidityConstructor :: Show SolidityConstructor where
  show = genericShow

instance decodeJsonSolidityConstructor :: DecodeJson SolidityConstructor where
  decodeJson json = do
    obj <- decodeJson json
    is <- obj .: "inputs"
    pure $ SolidityConstructor
      { inputs: is
      , isUnCurried: all (\(FunctionInput fi) -> fi.name /= "") is
          && not (null is)
      }

--------------------------------------------------------------------------------
-- | Solidity Events Parser
--------------------------------------------------------------------------------

newtype IndexedSolidityValue =
  IndexedSolidityValue
    { type :: SolidityType
    , name :: String
    , indexed :: Boolean
    }

derive instance genericSolidityIndexedValue :: Generic IndexedSolidityValue _

instance showSolidityIndexedValue :: Show IndexedSolidityValue where
  show = genericShow

instance formatIndexedSolidityValue :: Format IndexedSolidityValue where
  format (IndexedSolidityValue v) = format v.type

instance decodeJsonIndexedSolidityValue :: DecodeJson IndexedSolidityValue where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .: "name"
    ts <- obj .: "type"
    t <- parseSolidityType ts
    ixed <- obj .: "indexed"
    pure $ IndexedSolidityValue
      { name: nm
      , type: t
      , indexed: ixed
      }

newtype SolidityEvent =
  SolidityEvent
    { name :: String
    , anonymous :: Boolean
    , inputs :: Array IndexedSolidityValue
    }

derive instance genericSolidityEvent :: Generic SolidityEvent _

instance showSolidityEvent :: Show SolidityEvent where
  show = genericShow

instance decodeJsonSolidityEvent :: DecodeJson SolidityEvent where
  decodeJson json = do
    obj <- decodeJson json
    nm <- obj .: "name"
    is <- obj .: "inputs"
    a <- obj .: "anonymous"
    pure $ SolidityEvent
      { name: nm
      , inputs: is
      , anonymous: a
      }

data SolidityFallback = SolidityFallback

derive instance genericSolidityFallback :: Generic SolidityFallback _

instance showSolidityFallback :: Show SolidityFallback where
  show = genericShow

instance decodeJsonSolidityFallback :: DecodeJson SolidityFallback where
  decodeJson _ = do
    pure $ SolidityFallback

data SolidityReceive = SolidityReceive

derive instance genericSolidityReceive :: Generic SolidityReceive _

instance showSolidityReceive :: Show SolidityReceive where
  show = genericShow

instance decodeJsonSolidityReceive :: DecodeJson SolidityReceive where
  decodeJson _ = do
    pure $ SolidityReceive

--------------------------------------------------------------------------------
-- | ABI
--------------------------------------------------------------------------------

data AbiType
  = AbiFunction SolidityFunction
  | AbiConstructor SolidityConstructor
  | AbiEvent SolidityEvent
  | AbiFallback SolidityFallback
  | AbiReceive SolidityReceive

derive instance genericAbiType :: Generic AbiType _

instance showAbiType :: Show AbiType where
  show = genericShow

instance decodeJsonAbiType :: DecodeJson AbiType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    let json' = fromObject obj
    case t of
      "function" -> AbiFunction <$> decodeJson json'
      "constructor" -> AbiConstructor <$> decodeJson json'
      "event" -> AbiEvent <$> decodeJson json'
      "fallback" -> AbiFallback <$> decodeJson json'
      "receive" -> AbiReceive <$> decodeJson json'
      _ -> Left $ Named "Unkown abi type" $ UnexpectedValue json

newtype AbiDecodeError = AbiDecodeError { idx :: Int, error :: JsonDecodeError }

decodeArrayOfAbiTipes :: Json -> Either JsonDecodeError (Array (Either AbiDecodeError AbiType))
decodeArrayOfAbiTipes json = do
  arr <- note (Named "Failed to decode ABI as Array type." $ UnexpectedValue json) $ A.toArray json
  pure $ mapWithIndex safeDecode arr
  where
  safeDecode idx json' = decodeJson json' # lmap \error -> AbiDecodeError { idx, error }
