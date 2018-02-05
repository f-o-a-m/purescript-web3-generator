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
import Data.AbiParser (Abi(..), AbiType(..), IndexedSolidityValue(..), SolidityEvent(..), SolidityFunction(..), SolidityType(..), FunctionInput(..), format)
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (filter, length, mapWithIndex, replicate, uncons, unsafeIndex, zip, zipWith, (:), null)
import Data.Either (Either, either)
import Data.Foldable (fold, all)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.String (drop, fromCharArray, joinWith, singleton, take, toCharArray, toLower, toUpper)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), uncurry)
import Network.Ethereum.Web3.Types.Sha3 (sha3)
import Network.Ethereum.Web3.Types.Types (HexString, unHex)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile, readdir, mkdir, exists)
import Node.Path (FilePath, basenameWithoutExt, extname)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------

class Code a where
  genCode :: a -> GeneratorOptions -> String

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

toSignature :: SolidityFunction -> String
toSignature (SolidityFunction f) =
  let args = map (\i -> format i) f.inputs
  in f.name <> "(" <> joinWith "," args <> ")"

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
vectorLength n = "(" <> go n <> ")"
  where
    go m = if m == 0 then "Z" else "S (" <> go (m - 1) <> ")"

toPSType :: SolidityType -> String
toPSType s = case s of
    SolidityBool -> "Boolean"
    SolidityAddress -> "Address"
    SolidityUint n -> "(" <> "UIntN " <> makeDigits n <> ")"
    SolidityInt n -> "(" <> "IntN " <> makeDigits n <> ")"
    SolidityString -> "String"
    SolidityBytesN n -> "(" <> "BytesN " <> makeDigits n <> ")"
    SolidityBytesD -> "ByteString"
    SolidityVector ns a -> expandVector ns a
    SolidityArray a -> "(" <> "Array " <> toPSType a <> ")"
  where
    expandVector ns' a' = unsafePartial $ case uncons ns' of
      Just {head, tail} ->
        if length tail == 0
          then "(" <> "Vector " <> vectorLength head <> " " <> toPSType a' <> ")"
          else "(" <> "Vector " <> vectorLength head <> " " <> expandVector tail a' <> ")"


--------------------------------------------------------------------------------
-- | Data decleration, instances, and helpers
--------------------------------------------------------------------------------

-- | Data declaration
data FunTypeDecl =
  FunTypeDecl { signature :: String
              , factorTypes :: Array String
              , typeName :: String
              }

funToTypeDecl :: SolidityFunction -> GeneratorOptions -> FunTypeDecl
funToTypeDecl fun@(SolidityFunction f) opts =
  FunTypeDecl { typeName : capitalize $ opts.prefix <> f.name <> "Fn"
              , factorTypes : map toPSType (map (\(FunctionInput fi) -> fi.type) f.inputs)
              , signature: toSignature fun
              }

instance codeDataDecl :: Code FunTypeDecl where
  genCode (FunTypeDecl decl) _ =
    let nArgs = length decl.factorTypes
    in fold [ "type "
            , decl.typeName
            , " = "
            , "Tagged (SProxy \"" <> decl.signature <> "\") (Tuple" <> show nArgs <> " " <> joinWith " " decl.factorTypes <> ")"
            ]

--------------------------------------------------------------------------------
-- | Helper functions (asynchronous call/send)
--------------------------------------------------------------------------------

data HelperFunction =
    CurriedHelperFunction { signature :: Array String
                          , unpackExpr :: {name :: String, stockArgs :: Array String, payloadArgs :: Array String}
                          , payload :: String
                          , transport :: String
                          , constraints :: Array String
                          , quantifiedVars :: Array String
                          }
  | UnCurriedHelperFunction { signature :: Array String
                            , unpackExpr :: {name :: String, stockArgs :: Array String, stockArgsR :: Array String}
                            , constraints :: Array String
                            , quantifiedVars :: Array String
                            , whereClause :: String
                            }

funToHelperFunction :: Boolean -> SolidityFunction -> GeneratorOptions -> HelperFunction
funToHelperFunction isWhere fun@(SolidityFunction f) opts =
    let (FunTypeDecl decl) = funToTypeDecl fun opts
        inputs' = map (\(FunctionInput fi) -> fi.type) f.inputs
        sigPrefix = if f.constant then callSigPrefix else sendSigPrefix
        constraints = ["IsAsyncProvider p"]
        quantifiedVars = ["e", "p"]
        stockVars = if f.constant
                      then ["x0", "cm"]
                      else ["x0" ]
        offset = length stockVars
        conVars = mapWithIndex (\i _ -> "x" <> show (offset + i)) inputs'
        helperTransport = toTransportPrefix f.constant $ length f.outputs
        helperPayload = toPayload isWhere decl.typeName conVars
    in CurriedHelperFunction { signature : sigPrefix <> map toPSType inputs' <> [toReturnType f.constant $ map toPSType f.outputs]
                             , unpackExpr : {name : lowerCase $ opts.prefix <> f.name, stockArgs : stockVars, payloadArgs : conVars}
                             , payload : helperPayload
                             , transport : helperTransport
                             , constraints: constraints
                             , quantifiedVars: quantifiedVars
                             }
  where
    callSigPrefix = ["TransactionOptions", "ChainCursor"]
    sendSigPrefix = ["TransactionOptions"]

funToHelperFunction' :: SolidityFunction -> GeneratorOptions -> HelperFunction
funToHelperFunction' fun@(SolidityFunction f) opts =
    let (FunTypeDecl decl) = funToTypeDecl fun opts
        sigPrefix = if f.constant then callSigPrefix else sendSigPrefix
        constraints = ["IsAsyncProvider p"]
        quantifiedVars = ["e", "p"]
        stockVars = if f.constant
                      then ["x0", "cm"]
                      else ["x0"]
        returnType = toReturnType f.constant $ map toPSType f.outputs
    in UnCurriedHelperFunction { signature : sigPrefix <> [recordInput f.inputs] <> [returnType]
                               , unpackExpr : {name : lowerCase $ opts.prefix <> f.name, stockArgs : stockVars <> ["r"], stockArgsR : stockVars}
                               , constraints: constraints
                               , quantifiedVars: quantifiedVars
                               , whereClause: genCode (whereHelper decl sigPrefix f.inputs returnType) opts {indentationLevel = opts.indentationLevel + 4}
                               }
  where
    callSigPrefix = ["TransactionOptions", "ChainCursor"]
    sendSigPrefix = ["TransactionOptions"]
    tagInput (FunctionInput fi) = "Tagged (SProxy " <> "\"" <> fi.name <> "\") " <> toPSType fi.type
    recordInput fis = "{ " <> joinWith ", " (map (\(FunctionInput fi) -> fi.name <> ":: " <> toPSType fi.type) fis) <> " }"
    whereHelper d pre is ret = unsafePartial $ case funToHelperFunction true fun opts of
      CurriedHelperFunction helper -> CurriedHelperFunction helper { constraints = []
                                                                   , quantifiedVars = []
                                                                   , unpackExpr = helper.unpackExpr {name = helper.unpackExpr.name <> "'"}
                                                                   , signature = pre <> map tagInput is <> [ret]
                                                                   }

toTransportPrefix :: Boolean -> Int -> String
toTransportPrefix isCall outputCount =
  let fun = if isCall then "call" else "sendTx"
      modifier = if isCall && outputCount == 1 then "unTuple1 <$> " else ""
  in modifier <> fun

toPayload :: Boolean -> String -> Array String -> String
toPayload needsUntag typeName args =
  let n = length args
      args' = if needsUntag
                 then map (\s -> "(untagged " <> s <> " )") args
                 else args
  in "((tagged $ Tuple" <> show n <> " " <> joinWith " " args' <> ") :: " <> typeName <> ")"

toReturnType :: Boolean -> Array String -> String
toReturnType constant outputs =
  if not constant
     then "Web3 p e HexString"
     else "Web3 p e " <> case length outputs of
       0 -> "()"
       1 -> unsafePartial $ unsafeIndex outputs 0
       _ -> "(Tuple" <> show (length outputs) <> " " <> joinWith " " outputs <> ")"

instance codeHelperFunction :: Code HelperFunction where
  genCode (CurriedHelperFunction h) opts =
    let constraints = fold $ map (\c -> c <> " => ") h.constraints
        quantification = if h.quantifiedVars == [] then "" else "forall " <> joinWith " " h.quantifiedVars <> ". "
        decl = h.unpackExpr.name <> " :: " <> quantification <> constraints <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " (h.unpackExpr.stockArgs <> h.unpackExpr.payloadArgs)
        defR = h.transport <> " " <> joinWith " " h.unpackExpr.stockArgs <> " " <> h.payload
    in fold $ map (\s -> indentation <> s) [decl <> "\n", defL <> " = " <> defR]
    where
      indentation = fold $ replicate opts.indentationLevel " "
  genCode (UnCurriedHelperFunction h) _ =
    let constraints = fold $ map (\c -> c <> " => ") h.constraints
        quantification = if h.quantifiedVars == [] then "" else "forall " <> joinWith " " h.quantifiedVars <> ". "
        decl = h.unpackExpr.name <> " :: " <> quantification <> constraints <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " h.unpackExpr.stockArgs
        defR = "uncurryFields " <> " r $ " <> h.unpackExpr.name <> "'" <> " " <> joinWith " " h.unpackExpr.stockArgsR
    in fold [decl <> "\n", defL <> " = " <> defR <> "\n", "   where\n", h.whereClause]

--------------------------------------------------------------------------------

data EventDataDecl =
  EventDataDecl { constructor :: String
                , indexedTypes :: Array (Tuple String String)
                , nonIndexedTypes :: Array (Tuple String String)
                , recordType :: Array (Tuple String String)
                }

eventToDataDecl :: SolidityEvent -> EventDataDecl
eventToDataDecl (SolidityEvent ev) =
  let is = filter (\(IndexedSolidityValue sv) -> sv.indexed) ev.inputs
      nis = filter (\(IndexedSolidityValue sv) -> not sv.indexed) ev.inputs
  in EventDataDecl { constructor: ev.name
                   , indexedTypes: map (\(IndexedSolidityValue sv) -> (Tuple sv.name $ toPSType sv.type)) is
                   , nonIndexedTypes: map (\(IndexedSolidityValue sv) -> (Tuple sv.name $ toPSType sv.type)) nis
                   , recordType: map (\(IndexedSolidityValue sv) -> Tuple sv.name $ toPSType sv.type) ev.inputs
                   }


instance codeEventDataDecl :: Code EventDataDecl where
  genCode (EventDataDecl decl) _ =
    let recordField (Tuple label val) = label <> " :: " <> val
        newtypeDef = "newtype " <> decl.constructor <> " = " <> decl.constructor <> " {" <> joinWith "," (map recordField decl.recordType) <> "}"
        newtypeInstanceDecl = "derive instance newtype" <> decl.constructor <> " :: Newtype " <> decl.constructor <> " _"
    in joinWith "\n\n" [ newtypeDef
                       , newtypeInstanceDecl
                       ]


data EventGenericInstance =
  EventGenericInstance { instanceNames :: Array String
                       , instanceTypes :: Array String
                       , genericDefs :: Array String
                       , genericDeriving :: String
                       }

instance codeEventGenericInstance :: Code EventGenericInstance where
  genCode (EventGenericInstance i) _ =
    let headers = uncurry (\n t -> "instance " <> n <> " :: " <> t <> " where") <$> (zip i.instanceNames i.instanceTypes)
        eventGenerics = (\d -> "\t" <> d) <$> i.genericDefs
        instances = zipWith (\h g -> h <> "\n" <> g) headers eventGenerics
    in joinWith "\n\n" $ i.genericDeriving : instances

eventToEventGenericInstance :: SolidityEvent -> EventGenericInstance
eventToEventGenericInstance ev@(SolidityEvent e) =
  let EventDataDecl decl = eventToDataDecl ev
      capConst = capitalize decl.constructor
  in EventGenericInstance { instanceNames: (\n -> "eventGeneric" <> capConst <> n) <$> ["Show", "eq"]
                          , instanceTypes: (\t -> t <> " " <> capConst) <$> ["Show", "Eq"]
                          , genericDefs: ["show = GShow.genericShow", "eq = GEq.genericEq"]
                          , genericDeriving: "derive instance generic" <> capConst <> " :: G.Generic " <> capConst <> " _"
                          }

data EventDecodeInstance =
  EventDecodeInstance { indexedTuple :: String
                      , nonIndexedTuple :: String
                      , combinedType :: String
                      , anonymous :: Boolean
                      }

instance codeEventDecodeInstance :: Code EventDecodeInstance where
  genCode (EventDecodeInstance ev) _ =
    let indexedEventDecl = "instance indexedEvent" <> ev.combinedType <> " :: IndexedEvent " <> ev.indexedTuple <> " " <> ev.nonIndexedTuple <> " " <> ev.combinedType <> " where"
        indexedEventBody = "isAnonymous _ = " <> show ev.anonymous
   in joinWith "\n\n" [ joinWith "\n" [ indexedEventDecl
                                      , "  " <> indexedEventBody
                                      ]
                      ]
eventToDecodeEventInstance :: SolidityEvent -> EventDecodeInstance
eventToDecodeEventInstance event@(SolidityEvent ev) =
    let (EventDataDecl decl) = eventToDataDecl event
        indexed = "(Tuple" <> show (length decl.indexedTypes) <> " " <> joinWith " " (map taggedFactor decl.indexedTypes) <> ")"
        nonIndexed = "(Tuple" <> show (length decl.nonIndexedTypes) <> " " <> joinWith " " (map taggedFactor decl.nonIndexedTypes) <> ")"
   in EventDecodeInstance {indexedTuple: indexed, nonIndexedTuple: nonIndexed, combinedType: decl.constructor, anonymous: ev.anonymous}
  where
    taggedFactor (Tuple label value) = "(Tagged (SProxy \"" <> label <> "\") " <> value <> ")"


data EventFilterInstance =
  EventFilterInstance { instanceName :: String
                      , instanceType :: String
                      , filterDef :: String
                      }

instance codeEventFilterInstance :: Code EventFilterInstance where
  genCode (EventFilterInstance i) _ =
    let header = "instance " <> i.instanceName <> " :: EventFilter " <> i.instanceType <> " where"
        eventFilter = "\t" <> i.filterDef
    in joinWith "\n" [header, eventFilter]

eventId :: SolidityEvent -> HexString
eventId (SolidityEvent e) =
  let eventArgs = map (\a -> format a) e.inputs
  in sha3 $ e.name <> "(" <> joinWith "," eventArgs <> ")"

eventToEventFilterInstance :: SolidityEvent -> EventFilterInstance
eventToEventFilterInstance ev@(SolidityEvent e) =
  let EventDataDecl decl = eventToDataDecl ev
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
      ]
    ]


eventToEventCodeBlock :: SolidityEvent -> CodeBlock
eventToEventCodeBlock ev@(SolidityEvent e) =
  EventCodeBlock (eventToDataDecl ev) (eventToEventFilterInstance ev) (eventToDecodeEventInstance ev) (eventToEventGenericInstance ev)

--------------------------------------------------------------------------------

mkComment :: Array String -> String
mkComment cs = let sep = (fromCharArray $ replicate 80 '-') <> "\n"
               in  sep <> (joinWith "\n" $ map (\s -> "-- | " <> s) cs) <> "\n" <> sep

data CodeBlock =
    FunctionCodeBlock FunTypeDecl HelperFunction
  | EventCodeBlock EventDataDecl  EventFilterInstance EventDecodeInstance EventGenericInstance

funToFunctionCodeBlock :: SolidityFunction -> GeneratorOptions -> CodeBlock
funToFunctionCodeBlock fun@(SolidityFunction f) opts = FunctionCodeBlock (funToTypeDecl fun opts) $
    if isUnCurried f
      then funToHelperFunction' fun opts
      else funToHelperFunction false fun opts
  where
    isUnCurried f' = all (\(FunctionInput fi) -> fi.name /= "") f'.inputs && not (null f'.inputs)

instance codeFunctionCodeBlock :: Code CodeBlock where
  genCode (FunctionCodeBlock decl@(FunTypeDecl d) helper) opts =
    let header = mkComment [d.typeName]
    in joinWith "\n\n" [ header
                       , genCode decl opts
                       , genCode helper opts
                       ]
  genCode (EventCodeBlock decl@(EventDataDecl d) filterInst eventInst genericInst) opts =
    let header = mkComment [d.constructor]
    in joinWith "\n\n" [ header
                       , genCode decl opts
                       , genCode filterInst opts
                       , genCode eventInst opts
                       , genCode genericInst opts
                       ]

instance codeAbi :: Code Abi where
  genCode (Abi abi) opts = joinWith "\n\n" <<< map genCode' $ abi
    where
      genCode' :: AbiType -> String
      genCode' at = case at of
        AbiFunction f -> genCode (funToFunctionCodeBlock f opts) opts
        AbiEvent e -> genCode (eventToEventCodeBlock e) opts
        _ -> ""

--------------------------------------------------------------------------------
-- | Tools to read and write the files
--------------------------------------------------------------------------------

type GeneratorOptions = {jsonDir :: FilePath, pursDir :: FilePath, truffle :: Boolean, prefix :: String, indentationLevel :: Int}

imports :: String
imports = joinWith "\n" [ "import Prelude"
                        , "import Data.Functor.Tagged (Tagged, tagged, untagged)"
                        , "import Data.Generic.Rep as G"
                        , "import Data.Generic.Rep.Eq as GEq"
                        , "import Data.Generic.Rep.Show as GShow"
                        , "import Data.Lens ((.~))"
                        , "import Data.Maybe (Maybe(..))"
                        , "import Data.Newtype (class Newtype)"
                        , "import Data.Symbol (SProxy)"
                        , "import Network.Ethereum.Web3.Types.Types (HexString(..))"
                        , "import Network.Ethereum.Web3.Types (ChainCursor(..), Web3, BigNumber, TransactionOptions, _address, _topics, _fromBlock, _toBlock, defaultFilter)"
                        , "import Network.Ethereum.Web3.Provider (class IsAsyncProvider)"
                        , "import Network.Ethereum.Web3.Contract (class EventFilter, call, sendTx)"
                        , "import Network.Ethereum.Web3.Solidity"
                        , "import Type.Row (Cons, Nil, RLProxy(..))"
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
    writeTextFile UTF8 destFile $ genPSModuleStatement opts destFile <> "\n"
      <> imports <> "\n" 
      <> genCode abi opts

parseAbi :: forall r. {truffle :: Boolean | r} -> Json -> Either String Abi
parseAbi {truffle} abiJson = case truffle of
  false -> decodeJson abiJson
  true -> let mabi = abiJson ^? _Object <<< ix "abi"
          in note "truffle artifact missing abi field" mabi >>= decodeJson

genPSModuleStatement :: GeneratorOptions -> FilePath -> String
genPSModuleStatement opts fp = comment <> "\n" 
  <> "module Contracts." 
  <> basenameWithoutExt fp ".purs" 
  <> " where\n"
    where
  comment = mkComment [basenameWithoutExt fp ".purs"]
