module Data.AbiParser where

import Prelude

import Control.Alternative ((<|>))
import Control.Error.Util (note)
import Data.Argonaut ((.:?))
import Data.Argonaut as A
import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (fromFoldable, intercalate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
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

newtype NamedSolidityType =
  NamedSolidityType { name :: Maybe String, type :: SolidityType }

instance DecodeJson NamedSolidityType where
  decodeJson json = do
    obj <- decodeJson json
    _n <- obj .:? "name"
    let
      n = case _n of
        Just "" -> Nothing
        a -> a
    t' <- decodeJson json
    pure $ NamedSolidityType { name: n, type: t' }

derive instance Generic NamedSolidityType _
derive instance Eq NamedSolidityType

instance Show NamedSolidityType where
  show x = genericShow x

instance Format NamedSolidityType where
  format (NamedSolidityType { type: t }) = format t

data BasicSolidityType
  = SolidityBool
  | SolidityAddress
  | SolidityUint Int
  | SolidityInt Int
  | SolidityString
  | SolidityBytesN Int
  | SolidityBytesD
  | SolidityTuple (Array NamedSolidityType)

derive instance Generic BasicSolidityType _
derive instance Eq BasicSolidityType

instance Show BasicSolidityType where
  show x = genericShow x

instance Format BasicSolidityType where
  format s = case s of
    SolidityBool -> "bool"
    SolidityAddress -> "address"
    SolidityUint n -> "uint" <> show n
    SolidityInt n -> "int" <> show n
    SolidityString -> "string"
    SolidityBytesN n -> "bytes" <> show n
    SolidityBytesD -> "bytes"
    SolidityTuple as -> "(" <> intercalate "," (map (\(NamedSolidityType { type: t }) -> format t) as) <> ")"

data SolidityType
  = BasicType BasicSolidityType
  | SolidityVector Int SolidityType
  | SolidityArray SolidityType

derive instance Generic SolidityType _
derive instance Eq SolidityType

instance Show SolidityType where
  show x = genericShow x

instance DecodeJson SolidityType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    solType <- parseSolidityType t
    case solType of
      Left t' -> pure t'
      Right solidityTuple -> do
        components <- obj .: "components"
        factors <- traverse decodeJson components
        pure $ solidityTuple factors

instance Format SolidityType where
  format s = case s of
    BasicType t -> format t
    SolidityVector n a -> format a <> "[" <> show n <> "]"
    SolidityArray a -> format a <> "[]"

parseUint :: Parser BasicSolidityType
parseUint = do
  _ <- string "uint"
  n <- parseDigits >>= asInt
  pure $ SolidityUint n

parseInt :: Parser BasicSolidityType
parseInt = do
  _ <- string "int"
  n <- parseDigits >>= asInt
  pure $ SolidityInt n

parseBool :: Parser BasicSolidityType
parseBool = string "bool" >>= \_ -> pure SolidityBool

parseString :: Parser BasicSolidityType
parseString = string "string" >>= \_ -> pure SolidityString

parseDigits :: Parser String
parseDigits = fromCharArray <<< fromFoldable <$> many1 (try anyDigit)

asInt :: String -> Parser Int
asInt n = case fromString n of
  Nothing -> do
    fail $ "Couldn't parse as Int : " <> n
  Just n' -> pure $ n'

parseBytes :: Parser BasicSolidityType
parseBytes = do
  _ <- string "bytes"
  mns <- optionMaybe parseDigits
  case mns of
    Nothing -> pure SolidityBytesD
    Just ns -> do
      n <- asInt ns
      pure $ SolidityBytesN n

parseAddress :: Parser BasicSolidityType
parseAddress = string "address" >>= \_ -> pure SolidityAddress

solidityBasicTypeParser
  :: Parser
       ( Either
           BasicSolidityType
           (Array NamedSolidityType -> BasicSolidityType)
       )
solidityBasicTypeParser =
  let
    p1 = choice
      [ parseUint
      , parseInt
      , parseAddress
      , parseBool
      , parseString
      , parseBytes
      , parseAddress
      ]
    p2 = string "tuple" *> pure SolidityTuple
  in
    (Left <$> p1) <|> (Right <$> p2)

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

parseSolidityType
  :: String
  -> Either
       JsonDecodeError
       ( Either
           SolidityType
           (Array NamedSolidityType -> SolidityType)
       )
parseSolidityType s = parseSolidityType' s # lmap \err ->
  Named err $ UnexpectedValue (encodeJson s)

parseSolidityType'
  :: String
  -> Either
       String
       ( Either
           SolidityType
           (Array NamedSolidityType -> SolidityType)
       )
parseSolidityType' s = runParser (solidityTypeParser <* eof) s # lmap \err ->
  "Failed to parse SolidityType " <> show s <> " with error: " <> show err

solidityTypeParser
  :: Parser
       ( Either
           SolidityType
           (Array NamedSolidityType -> SolidityType)
       )
solidityTypeParser = do
  et <- solidityBasicTypeParser
  arrayTypes <- reverse <$> manyTill parseArrayType eof
  let
    f at acc = case at of
      Dynamic -> SolidityArray acc
      FixedLength n -> SolidityVector n acc
  pure $
    either
      (\t -> Left $ foldr f (BasicType t) arrayTypes)
      ( \solidityTuple -> Right
          \factors ->
            foldr f (BasicType $ solidityTuple factors) arrayTypes
      )
      et

--------------------------------------------------------------------------------
-- | Solidity Function Parser
--------------------------------------------------------------------------------

data SolidityFunction =
  SolidityFunction
    { name :: String
    , inputs :: Array NamedSolidityType
    , outputs :: Array NamedSolidityType
    , constant :: Boolean
    , payable :: Boolean
    , isConstructor :: Boolean
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
      }

--------------------------------------------------------------------------------
-- | Solidity Events Parser
--------------------------------------------------------------------------------

data IndexedSolidityValue =
  IndexedSolidityValue
    { type :: NamedSolidityType
    , indexed :: Boolean
    }

derive instance Generic IndexedSolidityValue _

instance Show IndexedSolidityValue where
  show = genericShow

instance Format IndexedSolidityValue where
  format (IndexedSolidityValue v) = format v.type

instance DecodeJson IndexedSolidityValue where
  decodeJson json = do
    t <- decodeJson json
    obj <- decodeJson json
    ixed <- obj .: "indexed"
    pure $ IndexedSolidityValue
      { type: t
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

data SolidityConstructor =
  SolidityConstructor
    { inputs :: Array NamedSolidityType
    }

derive instance Generic SolidityConstructor _

instance Show SolidityConstructor where
  show = genericShow

instance DecodeJson SolidityConstructor where
  decodeJson json = do
    obj <- decodeJson json
    is <- obj .: "inputs"
    pure $ SolidityConstructor
      { inputs: is }

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
