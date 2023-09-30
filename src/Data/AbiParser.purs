module Data.AbiParser where

import Prelude

import Control.Alternative ((<|>))
import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (decodeJson, (.:?))
import Data.Argonaut as A
import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (fromFoldable, intercalate, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (all, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List (reverse)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.TacitString as TacitString
import Data.Traversable (traverse)
import StringParser (Parser, fail, runParser, try)
import StringParser.CodePoints (anyDigit, string, char, eof)
import StringParser.Combinators (choice, manyTill, many1, optionMaybe)

--------------------------------------------------------------------------------

class Format a where
  format :: a -> String

--------------------------------------------------------------------------------
-- | Solidity Type Parsers
--------------------------------------------------------------------------------

newtype NamedSolidityType = NamedSolidityType { name :: Maybe String, type :: SolidityType }

instance DecodeJson NamedSolidityType where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .:? "name"
    t' <- decodeJson json
    pure $ NamedSolidityType { name: n, type: t' }

derive instance Generic NamedSolidityType _
derive instance Eq NamedSolidityType

instance Show NamedSolidityType where
  show x = genericShow x

data SolidityType
  = SolidityBool
  | SolidityAddress
  | SolidityUint Int
  | SolidityInt Int
  | SolidityString
  | SolidityBytesN Int
  | SolidityBytesD
  | SolidityVector Int SolidityType
  | SolidityArray SolidityType
  | SolidityTuple (Array NamedSolidityType)

derive instance Generic SolidityType _
derive instance Eq SolidityType

instance Show SolidityType where
  show x = genericShow x

instance Format SolidityType where
  format s = case s of
    SolidityBool -> "bool"
    SolidityAddress -> "address"
    SolidityUint n -> "uint" <> show n
    SolidityInt n -> "int" <> show n
    SolidityString -> "string"
    SolidityBytesN n -> "bytes" <> show n
    SolidityBytesD -> "bytes"
    SolidityVector n a -> format a <> "[" <> show n <> "]"
    SolidityArray a -> format a <> "[]"
    SolidityTuple as -> "(" <> intercalate "," (map (\(NamedSolidityType { type: t }) -> format t) as) <> ")"

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

parseBytes :: Parser SolidityType
parseBytes = do
  _ <- string "bytes"
  mns <- optionMaybe parseDigits
  case mns of
    Nothing -> pure SolidityBytesD
    Just ns -> do
      n <- asInt ns
      pure $ SolidityBytesN n

parseAddress :: Parser SolidityType
parseAddress = string "address" >>= \_ -> pure SolidityAddress

solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
  choice
    [ parseUint
    , parseInt
    , parseAddress
    , parseBool
    , parseString
    , parseBytes
    , parseAddress
    ]

data ArrayType = Dynamic | FixedLength Int

derive instance Generic ArrayType _

instance Show ArrayType where
  show x = genericShow x

derive instance Eq ArrayType

parseArrayType :: Parser ArrayType
parseArrayType = do
  let
    dynamic = do
      _ <- string "[]"
      pure Dynamic
    fixedLength = do
      _ <- char '['
      n <- parseDigits >>= asInt
      _ <- char ']'
      pure $ FixedLength n
  dynamic <|> fixedLength

solidityTypeParser :: Parser SolidityType
solidityTypeParser = do
  t <- solidityBasicTypeParser
  arrayTypes <- reverse <$> manyTill parseArrayType eof
  let
    f at acc = case at of
      Dynamic -> SolidityArray acc
      FixedLength n -> SolidityVector n acc
  pure $ foldr f t arrayTypes

parseSolidityType :: String -> Either JsonDecodeError SolidityType
parseSolidityType s = parseSolidityType' s # lmap \err -> Named err $ UnexpectedValue (encodeJson s)

parseSolidityType' :: String -> Either String SolidityType
parseSolidityType' s = runParser (solidityTypeParser <* eof) s # lmap \err ->
  "Failed to parse SolidityType " <> show s <> " with error: " <> show err

instance DecodeJson SolidityType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    if (t == "tuple") then do
      components <- obj .: "components"
      factors <- traverse decodeJson components
      pure $ SolidityTuple factors
    else parseSolidityType t

--------------------------------------------------------------------------------
-- | Solidity Function Parser
--------------------------------------------------------------------------------

newtype FunctionInput =
  FunctionInput
    { name :: String
    , type :: SolidityType
    }

derive instance Generic FunctionInput _

instance Show FunctionInput where
  show = genericShow

instance Format FunctionInput where
  format (FunctionInput fi) = format fi.type

instance DecodeJson FunctionInput where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .: "name"
    typ <- decodeJson json
    pure $ FunctionInput { type: typ, name: n }

data SolidityFunction =
  SolidityFunction
    { name :: String
    , inputs :: Array FunctionInput
    , outputs :: Array SolidityType
    , constant :: Boolean
    , payable :: Boolean
    , isConstructor :: Boolean
    , isUnCurried :: Boolean
    }

derive instance Generic SolidityFunction _

instance Show SolidityFunction where
  show = genericShow

instance DecodeJson SolidityFunction where
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

data SolidityConstructor =
  SolidityConstructor
    { inputs :: Array FunctionInput
    , isUnCurried :: Boolean
    }

derive instance Generic SolidityConstructor _

instance Show SolidityConstructor where
  show = genericShow

instance DecodeJson SolidityConstructor where
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

data IndexedSolidityValue =
  IndexedSolidityValue
    { type :: SolidityType
    , name :: String
    , indexed :: Boolean
    }

derive instance Generic IndexedSolidityValue _

instance Show IndexedSolidityValue where
  show = genericShow

instance Format IndexedSolidityValue where
  format (IndexedSolidityValue v) = format v.type

instance DecodeJson IndexedSolidityValue where
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

data SolidityEvent =
  SolidityEvent
    { name :: String
    , anonymous :: Boolean
    , inputs :: Array IndexedSolidityValue
    }

derive instance Generic SolidityEvent _

instance Show SolidityEvent where
  show = genericShow

instance DecodeJson SolidityEvent where
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

derive instance Generic SolidityFallback _

instance Show SolidityFallback where
  show = genericShow

instance DecodeJson SolidityFallback where
  decodeJson _ = do
    pure $ SolidityFallback

--------------------------------------------------------------------------------
-- | ABI
--------------------------------------------------------------------------------

data AbiType
  = AbiFunction SolidityFunction
  | AbiConstructor SolidityConstructor
  | AbiEvent SolidityEvent
  | AbiFallback SolidityFallback
  | Unknown

derive instance Generic AbiType _

instance Show AbiType where
  show = genericShow

instance DecodeJson AbiType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    let json' = fromObject obj
    case t of
      "function" -> AbiFunction <$> decodeJson json'
      "constructor" -> AbiConstructor <$> decodeJson json'
      "event" -> AbiEvent <$> decodeJson json'
      "fallback" -> AbiFallback <$> decodeJson json'
      _ -> pure Unknown

newtype Abi f = Abi (Array (f AbiType))

newtype AbiDecodeError = AbiDecodeError { idx :: Int, error :: String }

type AbiWithErrors = Abi (Either AbiDecodeError)

instance DecodeJson (Abi (Either AbiDecodeError)) where
  decodeJson json = do
    arr <- note (Named "Failed to decode ABI as Array type." $ UnexpectedValue json) $ A.toArray json
    pure $ Abi $ mapWithIndex safeDecode arr
    where
    safeDecode idx json' = decodeJson json' # lmap \error -> AbiDecodeError { idx, error: printJsonDecodeError error }

instance
  ( Functor f
  , Show (f TacitString.TacitString)
  ) =>
  Show (Abi f) where
  show (Abi abis) = "(Abi " <> show (map showFAbiType abis) <> ")"
    where
    showFAbiType = map (show >>> TacitString.hush)

instance Show AbiDecodeError where
  show (AbiDecodeError r) = "(AbiDecodeError " <> show r <> ")"
