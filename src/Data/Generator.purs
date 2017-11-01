module Data.Generator where

import Prelude

import Ansi.Codes (Color(Green))
import Ansi.Output (withGraphics, foreground)
import Control.Error.Util (note)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.AbiParser (Abi(..), AbiType(..), SolidityType(..), IndexedSolidityValue(..), SolidityFunction(..), SolidityEvent(..), format)
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Prisms (_Object)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, length, mapWithIndex, replicate, unsafeIndex)
import Data.Either (Either, either)
import Data.Foldable (fold)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.String (drop, fromCharArray, joinWith, singleton, take, toCharArray, toLower, toUpper)
import Data.Traversable (for)
import Network.Ethereum.Web3.Types (HexString(..), unHex, sha3)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile, readdir, mkdir, exists)
import Node.Path (FilePath, basenameWithoutExt, extname)
import Partial.Unsafe (unsafePartial)


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

makeDigits :: Int -> String
makeDigits n =
  let digits = map singleton <<< toCharArray <<< show $ n
      ddigits = map (\a -> "D" <> a) digits
      consed = joinWith " :& " ddigits
  in if length ddigits == 1
        then consed
        else "(" <> consed <> ")"

vectorLength :: Int -> String
vectorLength n = "N" <> show n

toPSType :: SolidityType -> String
toPSType s = case s of
  SolidityBool -> "Boolean"
  SolidityAddress -> "Address"
  SolidityUint n -> "(" <> "UIntN " <> makeDigits n <> ")"
  SolidityInt n -> "(" <> "IntN " <> makeDigits n <> ")"
  SolidityString -> "String"
  SolidityBytesN n -> "(" <> "BytesN " <> makeDigits n <> ")"
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
  DataDecl { constructor : capitalize $ "eth" <> f.name <> "Fn"
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
      DataDecl decl = funToDataDecl fun
  in BuilderMethod { unpackExpr : decl.constructor
                   , builderExpr : selectorBuilder
                   }

funToBuilderSomeArgs :: SolidityFunction -> BuilderMethod
funToBuilderSomeArgs fun@(SolidityFunction f) =
    let vars = mapWithIndex (\i _ -> "x"<> show i) f.inputs
        DataDecl decl = funToDataDecl fun
        selectorBuilder = "HexString " <> "\"" <> (unHex $ toSelector fun) <> "\""
        sep = " <> toDataBuilder "
        restBuilder = if length vars == 1
                         then " <> toDataBuilder " <> toSingleton (unsafePartial $ unsafeIndex vars 0)
                         else " <> toDataBuilder " <> toTuple vars
    in BuilderMethod { unpackExpr : "(" <> decl.constructor <> " " <> joinWith " " vars <> ")"
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
      DataDecl decl = funToDataDecl fun
  in  AbiEncodingInstance { instanceType : decl.constructor
                          , instanceName : "abiEncoding" <> decl.constructor
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
callSigPrefix = ["Address", "Maybe Address", "CallMode"]

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
                    , unpackExpr : {name : lowerCase $ "eth"<> f.name, stockArgs : stockVars, payloadArgs : conVars}
                    , payload : helperPayload
                    , transport : helperTransport
                    }

toTransportPrefix :: Boolean -> Int -> String
toTransportPrefix isCall outputCount =
  let fun = if isCall then "call" else "sendTx"
      modifier = if isCall && outputCount == 1 then "unSingleton <$> " else ""
  in modifier <> fun

toPayload :: String -> Array String -> String
toPayload constr args = case length args of
  0 -> constr
  _ -> "(" <> constr <> " " <> joinWith " " args <> ")"

toReturnType :: Boolean -> Array String -> String
toReturnType constant outputs =
  if not constant
     then "Web3 p e HexString"
     else "Web3 p e " <> case length outputs of
       0 -> "()"
       1 -> unsafePartial $ unsafeIndex outputs 0
       _ -> "(Tuple" <> show (length outputs) <> " " <> joinWith " " outputs <> ")"

instance codeHelperFunction :: Code HelperFunction where
  genCode (HelperFunction h) =
    let decl = h.unpackExpr.name <> " :: " <> "forall p e . IsAsyncProvider p => " <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " (h.unpackExpr.stockArgs <> h.unpackExpr.payloadArgs)
        defR = h.transport <> " " <> joinWith " " h.unpackExpr.stockArgs <> " " <> h.payload
    in decl <> "\n" <> defL <> " = " <> defR

--------------------------------------------------------------------------------

eventToDataDecl :: SolidityEvent -> DataDecl
eventToDataDecl (SolidityEvent ev) =
  DataDecl { constructor: ev.name
           , factorTypes: map (toPSType <<< \(IndexedSolidityValue sv) -> sv.type) ev.inputs
           }

data ParserMethod =
  ParserMethod { parserExpr :: String
               }

eventToParser :: SolidityEvent -> ParserMethod
eventToParser ev@(SolidityEvent e) =
  if length e.inputs == 0 then eventToParserNoArgs ev else eventToParserSomeArgs ev

eventToParserNoArgs :: SolidityEvent -> ParserMethod
eventToParserNoArgs ev@(SolidityEvent e) =
  let DataDecl decl = eventToDataDecl ev
  in ParserMethod { parserExpr : "pure " <> decl.constructor
                  }

eventToParserSomeArgs :: SolidityEvent -> ParserMethod
eventToParserSomeArgs ev@(SolidityEvent e) =
    let DataDecl decl = eventToDataDecl ev
        starter = if length decl.factorTypes == 1
                    then fromSingleton decl.constructor
                    else fromTuple decl.constructor (length decl.factorTypes)
    in ParserMethod { parserExpr: starter <> " <$> fromDataParser"
                    }
  where
    fromSingleton c = "uncurry1 " <> c
    fromTuple c n = "uncurry" <> show n <> " " <> c

eventToEncodingInstance :: SolidityEvent -> AbiEncodingInstance
eventToEncodingInstance ev@(SolidityEvent e) =
  let ParserMethod m = eventToParser ev
  in  AbiEncodingInstance { instanceType : capitalize e.name
                          , instanceName : "abiEncoding" <> capitalize e.name
                          , builder : "toDataBuilder = const mempty"
                          , parser : "fromDataParser = " <> m.parserExpr
                          }

data EventFilterInstance =
  EventFilterInstance { instanceName :: String
                      , instanceType :: String
                      , filterDef :: String
                      }

instance codeEventInstance :: Code EventFilterInstance where
  genCode (EventFilterInstance i) =
    let header = "instance " <> i.instanceName <> " :: EventFilter " <> i.instanceType <> " where"
        eventFilter = "\t" <> i.filterDef
    in joinWith "\n" [header, eventFilter]


eventId :: SolidityEvent -> HexString
eventId (SolidityEvent e) =
  let eventArgs = map (\a -> format a) e.inputs
  in sha3 $ e.name <> "(" <> joinWith "," eventArgs <> ")"

eventToEventFilterInstance :: SolidityEvent -> EventFilterInstance
eventToEventFilterInstance ev@(SolidityEvent e) =
    let DataDecl decl = eventToDataDecl ev
    in EventFilterInstance { instanceName: "eventFilter" <> capitalize decl.constructor
                           , instanceType: capitalize decl.constructor
                           , filterDef: "eventFilter _ addr = " <> mkFilterExpr "addr"
                           }
  where
    nIndexedArgs = length $ filter (\(IndexedSolidityValue v) -> v.indexed) e.inputs
    eventIdStr = "Just (" <> "HexString " <> "\"" <> (unHex $ eventId ev) <> "\"" <> ")"
    indexedVals = if nIndexedArgs == 0
                    then ""
                    else "," <> joinWith "," (replicate nIndexedArgs "Nothing")
    mkFilterExpr :: String -> String
    mkFilterExpr addr = fold
      [ "defaultFilter"
      , "\n\t\t"
      , joinWith "\n\t\t"
        [ "# _address .~ Just " <> addr
        , "# _topics .~ Just [" <> eventIdStr <> indexedVals <> "]"
        , "# _fromBlock .~ Nothing"
        , "# _toBlock .~ Nothing"
        ]
      ]

eventToEventCodeBlock :: SolidityEvent -> CodeBlock
eventToEventCodeBlock ev@(SolidityEvent e) =
  EventCodeBlock (eventToDataDecl ev) (eventToEncodingInstance ev) (eventToEventFilterInstance ev)

--------------------------------------------------------------------------------

data CodeBlock =
    FunctionCodeBlock DataDecl AbiEncodingInstance HelperFunction
  | EventCodeBlock DataDecl AbiEncodingInstance EventFilterInstance

funToFunctionCodeBlock :: SolidityFunction -> CodeBlock
funToFunctionCodeBlock f = FunctionCodeBlock (funToDataDecl f) (funToEncodingInstance f) (funToHelperFunction f)

instance codeFunctionCodeBlock :: Code CodeBlock where
  genCode (FunctionCodeBlock decl@(DataDecl d) inst helper) =
    let sep = fromCharArray $ replicate 80 '-'
        comment = "-- | " <> d.constructor
        header = sep <> "\n" <> comment <> "\n" <> sep
    in joinWith "\n\n" [ header
                       , genCode decl
                       , genCode inst
                       , genCode helper
                       ]
  genCode (EventCodeBlock decl@(DataDecl d) abiInst eventInst) =
    let sep = fromCharArray $ replicate 80 '-'
        comment = "-- | " <> d.constructor
        header = sep <> "\n" <> comment <> "\n" <> sep
    in joinWith "\n\n" [ header
                       , genCode decl
                       , genCode abiInst
                       , genCode eventInst
                       ]

instance codeAbi :: Code Abi where
  genCode (Abi abi) = joinWith "\n\n" <<< map genCode' $ abi
    where
      genCode' :: AbiType -> String
      genCode' at = case at of
        AbiFunction f -> genCode <<< funToFunctionCodeBlock $ f
        AbiEvent e -> genCode <<< eventToEventCodeBlock $ e
        _ -> ""

--------------------------------------------------------------------------------
-- | Tools to read and write the files
--------------------------------------------------------------------------------

type GeneratorOptions = {jsonDir :: FilePath, pursDir :: FilePath, truffle :: Boolean}

imports :: String
imports = joinWith "\n" [ "import Prelude"
                        , "import Data.Monoid (mempty)"
                        , "import Data.Lens ((.~))"
                        , "import Text.Parsing.Parser (fail)"
                        , "import Data.Maybe (Maybe(..))"
                        , "import Network.Ethereum.Web3.Types (HexString(..), CallMode, Web3, BigNumber, _address, _topics, _fromBlock, _toBlock, defaultFilter)"
                        , "import Network.Ethereum.Web3.Provider (class IsAsyncProvider)"
                        , "import Network.Ethereum.Web3.Contract (class EventFilter, call, sendTx)"
                        , "import Network.Ethereum.Web3.Solidity"
                        ]

generatePS :: forall e . GeneratorOptions -> Aff (fs :: FS, console :: CONSOLE | e) Unit
generatePS os = do
    let opts = os { pursDir = os.pursDir <> "/Contracts" }
    fs <- readdir opts.jsonDir
    isAlreadyThere <- exists opts.pursDir
    _ <- if isAlreadyThere then pure unit else mkdir opts.pursDir
    case fs of
      [] -> throwError <<< error $ "No abi json files found in directory: " <> opts.jsonDir
      fs' -> void $ for (filter (\f -> extname f == ".json") fs') $ \f -> do
        let f' = genPSFileName opts f
        writeCodeFromAbi opts (opts.jsonDir <> "/" <> f) f'
        let successCheck = withGraphics (foreground Green) $ "✔"
            successMsg = successCheck <> " contract module for " <> f <> " successfully written to " <> opts.pursDir
        liftEff <<< log $ successMsg
  where
    genPSFileName :: GeneratorOptions -> FilePath -> FilePath
    genPSFileName opts fp =
        opts.pursDir <> "/" <> basenameWithoutExt fp ".json" <> ".purs"

-- | read in json abi and write the generated code to a destination file
writeCodeFromAbi :: forall e . GeneratorOptions -> FilePath -> FilePath -> Aff (fs :: FS | e) Unit
writeCodeFromAbi opts abiFile destFile = do
    ejson <- jsonParser <$> readTextFile UTF8 abiFile
    json <- either (throwError <<< error) pure ejson
    (abi :: Abi) <- either (throwError <<< error) pure $ parseAbi opts json
    writeTextFile UTF8 destFile $
      genPSModuleStatement opts destFile <> "\n" <> imports <> "\n" <> genCode abi

parseAbi :: forall r. {truffle :: Boolean | r} -> Json -> Either String Abi
parseAbi {truffle} abiJson = case truffle of
  false -> decodeJson abiJson
  true -> let mabi = abiJson ^? _Object <<< ix "abi"
          in note "truffle artifact missing abi field" mabi >>= decodeJson

genPSModuleStatement :: GeneratorOptions -> FilePath -> String
genPSModuleStatement opts fp = "module Contracts." <> basenameWithoutExt fp ".purs" <> " where\n"
