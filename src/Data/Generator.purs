module Data.Generator where

import Prelude
import Data.String (fromCharArray, joinWith, take, drop, toUpper, toLower, replace, Replacement(..), Pattern(..))
import Data.Array (length, replicate, mapWithIndex, unsafeIndex, filter)
import Data.Either (either)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Class (liftEff)
import Partial.Unsafe (unsafePartial)
import Data.Argonaut (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Node.Encoding (Encoding(UTF8))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Aff (Aff)
import Node.FS.Aff (FS, readTextFile, writeTextFile, readdir, mkdir)
import Node.Path (FilePath, basenameWithoutExt, extname)
import Data.Traversable (for)

import Network.Ethereum.Web3.Types (HexString(..), unHex, sha3)
import Data.AbiParser (Abi(..), AbiType(..), SolidityType(..), SolidityFunction(..), format)

--------------------------------------------------------------------------------
class Code a where
  genCode :: a -> String

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------
toSelector :: SolidityFunction -> HexString
toSelector (SolidityFunction f) =
  let args = map (\i -> format i) f.inputs
      HexString hx = sha3 $ f.name <> "(" <> joinWith "," args <> ")"
  in HexString $ take 8 hx

capitalize :: String -> String
capitalize s =
  let h = toUpper $ take 1 s
      rest = drop 1 s
  in h <> rest

lowerCase :: String -> String
lowerCase s =
  let h = toLower $ take 1 s
      rest = drop 1 s
  in h <> rest

byteSizeDigits :: Int -> String
byteSizeDigits n =
  let ones = n `mod` 10
      tens = (n - ones) `div` 10
  in if tens == 0
       then "D" <> show ones
       else "(" <> "D" <> show tens <> " :& " <> "D" <> show ones <> ")"

vectorLength :: Int -> String
vectorLength n = "N" <> show n

toPSType :: SolidityType -> String
toPSType s = case s of
  SolidityBool -> "Boolean"
  SolidityAddress -> "Address"
  SolidityUint -> "BigNumber"
  SolidityString -> "String"
  SolidityBytesN n -> "(" <> "BytesN " <> byteSizeDigits n <> ")"
  SolidityBytesD -> "ByteString"
  SolidityVector n a ->  "(" <> "Vector " <> vectorLength n <> " " <> toPSType a <> ")"
  SolidityArray a -> "(" <> "Array " <> toPSType a <> ")"

--------------------------------------------------------------------------------
-- | Data decleration, instances, and helpers
--------------------------------------------------------------------------------

-- | Data declaration
data DataDecl =
  DataDecl { constructor :: String
           , factorTypes :: Array String
           }

funToDataDecl :: SolidityFunction -> DataDecl
funToDataDecl (SolidityFunction f) =
  DataDecl { constructor : capitalize f.name
           , factorTypes : map toPSType f.inputs
           }

instance codeDataDecl :: Code DataDecl where
  genCode (DataDecl decl) =
    "data " <> decl.constructor <> " = " <> decl.constructor <> " " <> joinWith " " decl.factorTypes

-- | Encoding instance
data BuilderMethod =
  BuilderMethod { unpackExpr :: String
                , builderExpr :: String
                }
funToBuilder :: SolidityFunction -> BuilderMethod
funToBuilder fun@(SolidityFunction f) =
  if length f.inputs == 0 then funToBuilderNoArgs fun else funToBuilderSomeArgs fun

funToBuilderNoArgs :: SolidityFunction -> BuilderMethod
funToBuilderNoArgs fun@(SolidityFunction f) =
  let selectorBuilder = "HexString " <> "\"" <> (unHex $ toSelector fun) <> "\""
  in BuilderMethod { unpackExpr : capitalize f.name
                   , builderExpr : selectorBuilder
                   }

funToBuilderSomeArgs :: SolidityFunction -> BuilderMethod
funToBuilderSomeArgs fun@(SolidityFunction f) =
    let vars = mapWithIndex (\i _ -> "x"<> show i) f.inputs
        selectorBuilder = "HexString " <> "\"" <> (unHex $ toSelector fun) <> "\""
        sep = " <> toDataBuilder "
        restBuilder = if length vars == 1
                         then " <> toDataBuilder " <> toSingleton (unsafePartial $ unsafeIndex vars 0)
                         else " <> toDataBuilder " <> toTuple vars
    in BuilderMethod { unpackExpr : "(" <> capitalize f.name <> " " <> joinWith " " vars <> ")"
                     , builderExpr : selectorBuilder <> restBuilder
                     }
  where
    toSingleton x = "(Singleton " <> x <> ")"
    toTuple vars = "(Tuple" <> show (length vars) <> " " <> joinWith " " vars <> ")"

data AbiEncodingInstance =
  AbiEncodingInstance { instanceType :: String
                      , instanceName :: String
                      , builder :: String
                      , parser :: String
                      }

funToEncodingInstance :: SolidityFunction -> AbiEncodingInstance
funToEncodingInstance fun@(SolidityFunction f) =
  let BuilderMethod m = funToBuilder fun
  in  AbiEncodingInstance { instanceType : capitalize f.name
                          , instanceName : "abiEncoding" <> capitalize f.name
                          , builder : "toDataBuilder " <> m.unpackExpr <> " = " <> m.builderExpr
                          , parser : "fromDataParser = fail \"Function type has no parser.\""
                          }

instance codeAbiEncodingInstance :: Code AbiEncodingInstance where
  genCode (AbiEncodingInstance i) =
    let header = "instance " <> i.instanceName <> " :: ABIEncoding " <> i.instanceType <> " where"
        bldr = "\t" <> i.builder
        prsr = "\t" <> i.parser
    in joinWith "\n" [header, bldr, prsr]

--------------------------------------------------------------------------------
-- | Helper functions (asynchronous call/send)
--------------------------------------------------------------------------------

callSigPrefix :: Array String
callSigPrefix = ["Maybe Address", "Address", "CallMode"]

sendSigPrefix :: Array String
sendSigPrefix = ["Maybe Address", "Address", "BigNumber"]

data HelperFunction =
  HelperFunction { signature :: Array String
                 , unpackExpr :: {name :: String, stockArgs :: Array String, payloadArgs :: Array String}
                 , payload :: String
                 , transport :: String
                 }

funToHelperFunction :: SolidityFunction -> HelperFunction
funToHelperFunction fun@(SolidityFunction f) =
  let (DataDecl decl) = funToDataDecl fun
      sigPrefix = if f.constant then callSigPrefix else sendSigPrefix
      stockVars = mapWithIndex (\i _ -> "x" <> show i) sigPrefix
      offset = length stockVars
      conVars = mapWithIndex (\i _ -> "x" <> show (offset + i)) f.inputs
      helperTransport = toTransportPrefix f.constant $ length f.outputs
      helperPayload = toPayload decl.constructor conVars
  in HelperFunction { signature : sigPrefix <> map toPSType f.inputs <> [toReturnType f.constant $ map toPSType f.outputs]
                    , unpackExpr : {name : f.name, stockArgs : stockVars, payloadArgs : conVars}
                    , payload : helperPayload
                    , transport : helperTransport
                    }

toTransportPrefix :: Boolean -> Int -> String
toTransportPrefix isCall outputCount =
  let fun = if isCall then "callAsync" else "sendTxAsync"
      modifier = if outputCount == 1 then "unSingleton <$> " else ""
  in modifier <> fun

toPayload :: String -> Array String -> String
toPayload constr args = case length args of
  0 -> constr
  _ -> "(" <> constr <> " " <> joinWith " " args <> ")"

toReturnType :: Boolean -> Array String -> String
toReturnType constant outputs =
  if not constant
     then "Web3MA () HexString"
     else "Web3MA () " <> case length outputs of
       0 -> "()"
       1 -> unsafePartial $ unsafeIndex outputs 0
       _ -> "(Tuple" <> show (length outputs) <> " " <> joinWith " " outputs <> ")"

instance codeHelperFunction :: Code HelperFunction where
  genCode (HelperFunction h) =
    let decl = h.unpackExpr.name <> " :: " <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " (h.unpackExpr.stockArgs <> h.unpackExpr.payloadArgs)
        defR = h.transport <> " " <> joinWith " " h.unpackExpr.stockArgs <> " " <> h.payload
    in decl <> "\n" <> defL <> " = " <> defR


--------------------------------------------------------------------------------

data FunctionCodeBlock = FunctionCodeBlock DataDecl AbiEncodingInstance HelperFunction

funToFunctionCodeBlock :: SolidityFunction -> FunctionCodeBlock
funToFunctionCodeBlock f = FunctionCodeBlock (funToDataDecl f) (funToEncodingInstance f) (funToHelperFunction f)

instance codeFunctionCodeBlock :: Code FunctionCodeBlock where
  genCode (FunctionCodeBlock decl@(DataDecl d) inst helper) =
    let sep = fromCharArray $ replicate 80 '-'
        comment = "-- | " <> d.constructor
        header = sep <> " \n" <> comment <> "\n" <> sep
    in joinWith "\n\n" [ header
                       , genCode decl
                       , genCode inst
                       , genCode helper
                       ]

instance codeAbi :: Code Abi where
  genCode (Abi abi) = joinWith "\n\n" <<< map genCode' $ abi
    where
      genCode' :: AbiType -> String
      genCode' at = case at of
        AbiFunction f -> genCode <<< funToFunctionCodeBlock $ f
        _ -> ""

--------------------------------------------------------------------------------
-- | Tools to read and write the files
--------------------------------------------------------------------------------

-- | read in json abi and write the generated code to a destination file
writeCodeFromAbi :: forall e . FilePath -> FilePath -> Aff (fs :: FS | e) Unit
writeCodeFromAbi abiFile destFile = do
  ejson <- jsonParser <$> readTextFile UTF8 abiFile
  json <- either (throwError <<< error) pure ejson
  (abi :: Abi) <- either (throwError <<< error) pure $ decodeJson json
  writeTextFile UTF8 destFile $ genCode abi

genPSModuleName :: FilePath -> FilePath -> FilePath -> FilePath
genPSModuleName jsonRoot pursRoot fp =
    pursRoot <> "/" <> basenameWithoutExt fp' ".json" <> ".purs"
  where
    fp' = replace (Pattern jsonRoot) (Replacement pursRoot) fp

findAllAbis :: forall e . FilePath -> FilePath -> Aff (fs :: FS, console :: CONSOLE | e) Unit
findAllAbis jsonRoot psRoot = do
  fs <- readdir jsonRoot
  case fs of
    [] -> throwError <<< error $ "No abi json files found in directory: " <> jsonRoot
    fs' -> void $ for (filter (\f -> extname f == ".json") fs') $ \f -> do
      let f' = genPSModuleName jsonRoot psRoot f
      liftEff $ logShow f
      liftEff $ logShow f'
      writeCodeFromAbi (jsonRoot <> "/" <> f) f'
