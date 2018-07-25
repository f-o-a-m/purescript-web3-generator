module Data.AbiParser where

import Prelude

import Control.Alternative ((<|>))
import Control.Error.Util (note)
import Data.Argonaut as A
import Data.Argonaut.Core (fromObject)
import Data.Argonaut.Decode ((.?))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array (fromFoldable, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (all, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic (class Generic, gEq, gShow)
import Data.Int (fromString)
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Record.Extra (showRecord)
import Data.String (fromCharArray)
import Data.TacitString as TacitString
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.Combinators (choice, lookAhead, manyTill, many1, optionMaybe)
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
  | SolidityVector (NonEmptyList Int) SolidityType
  | SolidityArray SolidityType

derive instance genericSolidityType :: Generic SolidityType

instance showSolidityType :: Show SolidityType where
  show = gShow

instance eqSolidityType :: Eq SolidityType where
  eq = gEq

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
    choice [ parseUint
           , parseInt
           , parseAddress
           , parseBool
           , parseString
           , parseBytes
           , parseAddress
           ]

vectoDimentionsParser :: Parser (List Int)
vectoDimentionsParser = manyTill
  (char '[' *> (parseDigits >>= asInt) <* char ']')
  (lookAhead $ void (string "[]") <|> eof)

solidityTypeParser :: Parser SolidityType
solidityTypeParser = do
  t <- solidityBasicTypeParser
  mbVectorDims <- vectoDimentionsParser
  let 
    t' = case mbVectorDims of
      Nil -> t
      Cons n ns -> SolidityVector (NonEmptyList $ n :| ns) t
  (SolidityArray t' <$ string "[]") <|> pure t'

parseSolidityType :: String -> Either String SolidityType
parseSolidityType s = runParser (solidityTypeParser <* eof) s # lmap \err ->
  "Failed to parse SolidityType " <> show s <> " with error: " <> show err

instance decodeJsonSolidityType :: DecodeJson SolidityType where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "type"
    parseSolidityType t

--------------------------------------------------------------------------------
-- | Solidity Function Parser
--------------------------------------------------------------------------------

newtype FunctionInput =
  FunctionInput { name :: String
                , type :: SolidityType
                }

derive instance genericFunctionInput :: Generic FunctionInput

instance showFunctionInput :: Show FunctionInput where
  show = gShow

instance formatInput :: Format FunctionInput where
  format (FunctionInput fi) = format fi.type

instance decodeFunctionInput :: DecodeJson FunctionInput where
  decodeJson json = do
    obj <- decodeJson json
    n <- obj .? "name"
    t <- obj .? "type"
    typ <- parseSolidityType t
    pure $ FunctionInput {type: typ, name: n}

data SolidityFunction =
  SolidityFunction { name :: String
                   , inputs :: Array FunctionInput
                   , outputs :: Array SolidityType
                   , constant :: Boolean
                   , payable :: Boolean
                   , isConstructor :: Boolean
                   , isUnCurried :: Boolean
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
                            , isConstructor: false
                            , isUnCurried: all (\(FunctionInput fi) -> fi.name /= "") is
                                             && not (null is)
                            }

--------------------------------------------------------------------------------
-- | Solidity Constructor Parser
--------------------------------------------------------------------------------

data SolidityConstructor =
  SolidityConstructor { inputs :: Array FunctionInput
                      , isUnCurried :: Boolean
                      }

derive instance genericSolidityConstructor :: Generic SolidityConstructor

instance showSolidityConstructor :: Show SolidityConstructor where
  show = gShow

instance decodeJsonSolidityConstructor :: DecodeJson SolidityConstructor where
  decodeJson json = do
    obj <- decodeJson json
    is <- obj .? "inputs"
    pure $ SolidityConstructor { inputs : is
                               , isUnCurried: all (\(FunctionInput fi) -> fi.name /= "") is
                                   && not (null is)
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


newtype Abi f = Abi (Array (f AbiType))

newtype AbiDecodeError = AbiDecodeError { idx :: Int, error :: String }

type AbiWithErrors = Abi (Either AbiDecodeError)

instance decodeJsonAbi :: DecodeJson (Abi (Either AbiDecodeError)) where
  decodeJson json = do
    arr <- note "Failed to decode ABI as Array type." $ A.toArray json
    pure $ Abi $ mapWithIndex safeDecode arr
    where
    safeDecode idx json' = decodeJson json' # lmap \error -> AbiDecodeError {idx , error}

instance showAbi ::
  ( Functor f
  , Show (f TacitString.TacitString)
  ) => Show (Abi f) where
  show (Abi abis) = "(Abi " <> show (map showFAbiType abis) <> ")"
    where
      showFAbiType = map (show >>> TacitString.hush)

instance showAbiDecodeError :: Show AbiDecodeError where
  show (AbiDecodeError r) = "(AbiDecodeError " <> showRecord r <> ")"
