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
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.AbiParser (Abi(..), AbiType(..), FunctionInput(..), IndexedSolidityValue(..), SolidityEvent(..), SolidityFunction(..), SolidityType(..), format)
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (filter, length, mapWithIndex, nub, null, replicate, sort, uncons, unsafeIndex, zip, zipWith, (:))
import Data.Either (Either, either)
import Data.Foldable (all, fold, foldl)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Map (Map, fromFoldableWith, insert, lookup, member, toAscUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard, mempty)
import Data.String (drop, fromCharArray, joinWith, singleton, take, toCharArray, toLower, toUpper)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Network.Ethereum.Web3.Types.Sha3 (sha3)
import Network.Ethereum.Web3.Types.Types (HexString, unHex)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile, readdir, mkdir, exists)
import Node.Path (FilePath, basenameWithoutExt, extname)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
type ModuleName = String
type ModuleImports = Array ModuleImport

data ModuleImport
  = IType String
  | ITypeCtr String
  | ITypeOp String
  | IClass String
  | IVal String
  | IOp String

type Imports = Array (Tuple ModuleName ModuleImports)
type Imported = Writer Imports

class Code a where
  genCode :: a -> GeneratorOptions -> Imported String

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

makeDigits :: Int -> Imported String
makeDigits n = do
  let digits = map singleton <<< toCharArray <<< show $ n
  ddigits <- for digits \d -> do
    let d' = "D" <> d
    import' "Network.Ethereum.Web3.Solidity" [IType d']
    pure d'
  let consed = joinWith " :& " ddigits
  if length ddigits == 1
    then pure consed
    else do 
    import' "Network.Ethereum.Web3.Solidity.Size" [ITypeOp ":&"]
    pure $ "(" <> consed <> ")"

import' :: ModuleName -> ModuleImports -> Imported Unit
import' mName mImports = tell [ Tuple mName mImports ]

vectorLength :: Int -> Imported String
vectorLength n = do
  import' "Network.Ethereum.Web3.Solidity" $ [IType "Z"] <> guard (n > 0) [IType "S"]
  pure $ "(" <> go n <> ")"
  where
    go m = if m == 0 then "Z" else "S (" <> go (m - 1) <> ")"

toPSType :: SolidityType -> Imported String
toPSType s = case s of
    SolidityBool -> do
      pure "Boolean"
    SolidityAddress -> do
      import' "Network.Ethereum.Web3.Types.Types" [IType "Address"]
      pure "Address"
    SolidityUint n -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "UIntN"]
      digits <- makeDigits n
      pure $ "(" <> "UIntN " <> digits <> ")"
    SolidityInt n -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "IntN"]
      digits <- makeDigits n
      pure $ "(" <> "IntN " <> digits <> ")"
    SolidityString -> do
      pure "String"
    SolidityBytesN n -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "BytesN"]
      digits <- makeDigits n
      pure $ "(" <> "BytesN " <> digits <> ")"
    SolidityBytesD -> do
      import' "Network.Ethereum.Web3.Solidity" [IType "ByteString"]
      pure "ByteString"
    SolidityVector ns a -> do
      expandVector ns a
    SolidityArray a -> do
      t <- toPSType a
      pure $ "(" <> "Array " <> t <> ")"
  where
  expandVector ns' a' = unsafePartial $ case uncons ns' of
    Just {head, tail} -> do
      l <- vectorLength head
      import' "Network.Ethereum.Web3" [IType "Vector"]
      if length tail == 0
        then do
          x <- toPSType a'
          pure $ "(" <> "Vector " <> l <> " " <> x <> ")"
        else do
          x <- expandVector tail a'
          pure $ "(" <> "Vector " <> l <> " " <> x <> ")"

--------------------------------------------------------------------------------
-- | Data decleration, instances, and helpers
--------------------------------------------------------------------------------

-- | Data declaration
data FunTypeDecl =
  FunTypeDecl { signature :: String
              , factorTypes :: Array String
              , typeName :: String
              }

funToTypeDecl :: SolidityFunction -> GeneratorOptions -> Imported FunTypeDecl
funToTypeDecl fun@(SolidityFunction f) opts = do
  factorTypes <- for f.inputs $ \(FunctionInput fi) ->
    toPSType $ fi.type
  pure $
    FunTypeDecl
      { typeName : capitalize $ opts.prefix <> f.name <> "Fn"
      , factorTypes
      , signature: toSignature fun
      }

instance codeDataDecl :: Code FunTypeDecl where
  genCode (FunTypeDecl decl) _ = do
    let
      nArgs = length decl.factorTypes
      tupleType = "Tuple" <> show nArgs
    import' "Data.Functor.Tagged" [IType "Tagged"]
    import' "Data.Symbol" [IType "SProxy"]
    import' "Network.Ethereum.Web3.Solidity" [ITypeCtr tupleType]
    pure $
      fold
        ["type "
        , decl.typeName
        , " = "
        , "Tagged (SProxy \"" <> decl.signature <> "\") (" <> tupleType <> " " <> joinWith " " decl.factorTypes <> ")"
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

funToHelperFunction :: Boolean -> SolidityFunction -> GeneratorOptions -> Imported HelperFunction
funToHelperFunction isWhereClause fun@(SolidityFunction f) opts = do
  (FunTypeDecl decl) <- funToTypeDecl fun opts
  import' "Network.Ethereum.Web3.Provider" [IClass "IsAsyncProvider"]
  import' "Network.Ethereum.Web3.Types" [IType "TransactionOptions"]
  sigPrefix <- if f.constant
    then do
      import' "Network.Ethereum.Web3.Types" [IType "ChainCursor"]
      pure ["TransactionOptions", "ChainCursor"]
    else
      pure ["TransactionOptions"]
  let
    var = if isWhereClause then "y" else "x"
    constraints = ["IsAsyncProvider p"]
    quantifiedVars = ["e", "p"]
    stockVars = if f.constant
                  then [var <> "0", "cm"]
                  else [var <> "0"]
    offset = length stockVars
    inputs' = map (\(FunctionInput fi) -> fi.type) f.inputs
    conVars = mapWithIndex (\i _ -> var <> show (offset + i)) inputs'
  helperTransport <- toTransportPrefix f.constant $ length f.outputs
  helperPayload <- toPayload isWhereClause decl.typeName conVars
  returnType <- toReturnType f.constant f.outputs
  ins <- for f.inputs $ \(FunctionInput fi) -> toPSType fi.type
  pure $
    CurriedHelperFunction { signature : sigPrefix <> ins <> [returnType]
                          , unpackExpr : {name : lowerCase $ opts.prefix <> f.name, stockArgs : stockVars, payloadArgs : conVars}
                          , payload : helperPayload
                          , transport : helperTransport
                          , constraints: constraints
                          , quantifiedVars: quantifiedVars
                          }

funToHelperFunction' :: SolidityFunction -> GeneratorOptions -> Imported HelperFunction
funToHelperFunction' fun@(SolidityFunction f) opts = do
    (FunTypeDecl decl) <- funToTypeDecl fun opts
    import' "Network.Ethereum.Web3.Provider" [IClass "IsAsyncProvider"]
    import' "Network.Ethereum.Web3.Types" [IType "TransactionOptions"]
    sigPrefix <- if f.constant
      then do
        import' "Network.Ethereum.Web3.Types" [IType "ChainCursor"]
        pure ["TransactionOptions", "ChainCursor"]
      else
        pure ["TransactionOptions"]
    let
      constraints = ["IsAsyncProvider p"]
      quantifiedVars = ["e", "p"]
      stockVars = if f.constant
                    then ["x0", "cm"]
                    else ["x0"]
    returnType <- toReturnType f.constant f.outputs
    recIn <- recordInput f.inputs
    whereC <- whereHelper decl sigPrefix f.inputs returnType >>= \h -> genCode h opts {indentationLevel = opts.indentationLevel + 4}
    pure $
      UnCurriedHelperFunction { signature : sigPrefix <> [recIn, returnType]
                              , unpackExpr : {name : lowerCase $ opts.prefix <> f.name, stockArgs : stockVars <> ["r"], stockArgsR : stockVars}
                              , constraints: constraints
                              , quantifiedVars: quantifiedVars
                              , whereClause: whereC
                              }
  where
    tagInput (FunctionInput fi) = do
      ty <- toPSType fi.type
      pure $ "Tagged (SProxy " <> "\"" <> fi.name <> "\") " <> ty
    recordInput fis = do
      rowElems <- for fis $ \(FunctionInput fi) -> do
        ty <- toPSType fi.type
        pure $ fi.name <> " :: " <> ty
      pure $ "{ " <> joinWith ", " rowElems <> " }"
    whereHelper d pre is ret = do
      hlpr <- funToHelperFunction true fun opts
      tys <- traverse tagInput is
      pure $ unsafePartial $ case hlpr of
        CurriedHelperFunction helper -> CurriedHelperFunction helper { constraints = []
                                                                     , quantifiedVars = []
                                                                     , unpackExpr = helper.unpackExpr {name = helper.unpackExpr.name <> "'"}
                                                                     , signature = pre <> tys <> [ret]
                                                                     }

toTransportPrefix :: Boolean -> Int -> Imported String
toTransportPrefix isCall outputCount = do
  fun <- if isCall
    then do
      import' "Network.Ethereum.Web3" [IVal "call"]
      pure "call"
    else do
      import' "Network.Ethereum.Web3" [IVal "sendTx"]
      pure "sendTx"
  modifier <- if isCall && outputCount == 1
    then do
      import' "Network.Ethereum.Web3.Solidity" [IVal "unTuple1"]
      pure $ if isCall
               then "map unTuple1 <$> "
               else "unTuple1 <$> "
    else
      pure ""
  pure $ modifier <> fun

toPayload :: Boolean -> String -> Array String -> Imported String
toPayload isWhereClause typeName args = do
  import' "Data.Functor.Tagged" [IVal "tagged"]
  let tupleType = "Tuple" <> show (length args)
  import' "Network.Ethereum.Web3.Solidity" [ITypeCtr tupleType]
  args' <- if isWhereClause
            then do
              import' "Data.Functor.Tagged" [IVal "untagged"]
              pure $ map (\s -> "(untagged " <> s <> " )") args
            else pure args

  pure $ "((tagged $ " <> tupleType <> " " <> joinWith " " args' <> ") :: " <> typeName <> ")"

toReturnType :: Boolean -> Array SolidityType -> Imported String
toReturnType constant outputs' = do
  import' "Network.Ethereum.Web3.Types.Types" [IType "Web3"]
  if not constant
    then do
      import' "Network.Ethereum.Web3.Types.Types" [IType "HexString"]
      pure "Web3 p e HexString"
    else do
      import' "Network.Ethereum.Web3.Types" [IType "CallError"]
      import' "Data.Either" [IType "Either"]
      outputs <- for outputs' toPSType
      out <- case length outputs of
        0 -> pure "Unit"
        1 -> pure $ unsafePartial $ unsafeIndex outputs 0
        _ -> do
          let tupleType = "Tuple" <> show (length outputs)
          import' "Network.Ethereum.Web3.Solidity" [IType tupleType]
          pure $ tupleType <> " " <> joinWith " " outputs
      pure $ "Web3 p e " <> "(Either CallError (" <> out <> "))"

instance codeHelperFunction :: Code HelperFunction where
  genCode (CurriedHelperFunction h) opts =
    let constraints = fold $ map (\c -> c <> " => ") h.constraints
        quantification = if h.quantifiedVars == [] then "" else "forall " <> joinWith " " h.quantifiedVars <> ". "
        decl = h.unpackExpr.name <> " :: " <> quantification <> constraints <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " (h.unpackExpr.stockArgs <> h.unpackExpr.payloadArgs)
        defR = h.transport <> " " <> joinWith " " h.unpackExpr.stockArgs <> " " <> h.payload
    in pure <<< fold $ map (\s -> indentation <> s) [decl <> "\n", defL <> " = " <> defR]
    where
      indentation = fold $ replicate opts.indentationLevel " "
  genCode (UnCurriedHelperFunction h) _ = do
    import' "Network.Ethereum.Web3.Contract.Internal" [IVal "uncurryFields"]
    let constraints = fold $ map (\c -> c <> " => ") h.constraints
        quantification = if h.quantifiedVars == [] then "" else "forall " <> joinWith " " h.quantifiedVars <> ". "
        decl = h.unpackExpr.name <> " :: " <> quantification <> constraints <> joinWith " -> " h.signature
        defL = h.unpackExpr.name <> " " <> joinWith " " h.unpackExpr.stockArgs
        defR = "uncurryFields " <> " r $ " <> h.unpackExpr.name <> "'" <> " " <> joinWith " " h.unpackExpr.stockArgsR
    pure <<< fold $ [decl <> "\n", defL <> " = " <> defR <> "\n", "   where\n", h.whereClause]

--------------------------------------------------------------------------------

data EventDataDecl =
  EventDataDecl { constructor :: String
                , indexedTypes :: Array (Tuple String String)
                , nonIndexedTypes :: Array (Tuple String String)
                , recordType :: Array (Tuple String String)
                }

eventToDataDecl :: SolidityEvent -> Imported EventDataDecl
eventToDataDecl (SolidityEvent ev) = do
  let is = filter (\(IndexedSolidityValue sv) -> sv.indexed) ev.inputs
      nis = filter (\(IndexedSolidityValue sv) -> not sv.indexed) ev.inputs
  indexedTypes <- for is \(IndexedSolidityValue sv) -> do
    t <- toPSType sv.type
    pure $ Tuple sv.name t
  nonIndexedTypes <- for nis \(IndexedSolidityValue sv) -> do
    t <- toPSType sv.type
    pure $ Tuple sv.name t
  recordType <- for ev.inputs \(IndexedSolidityValue sv) -> do
    t <- toPSType sv.type
    pure $ Tuple sv.name t
  pure $  EventDataDecl
    { constructor: ev.name
    , indexedTypes
    , nonIndexedTypes
    , recordType
    }


instance codeEventDataDecl :: Code EventDataDecl where
  genCode (EventDataDecl decl) _ = do
    import' "Data.Newtype" [IClass "Newtype"]
    let recordField (Tuple label val) = label <> " :: " <> val
        newtypeDef = "newtype " <> decl.constructor <> " = " <> decl.constructor <> " {" <> joinWith "," (map recordField decl.recordType) <> "}"
        newtypeInstanceDecl = "derive instance newtype" <> decl.constructor <> " :: Newtype " <> decl.constructor <> " _"
    pure $ 
      newLine2
        [ newtypeDef
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
    in pure $ newLine2 $ i.genericDeriving : instances

eventToEventGenericInstance :: SolidityEvent -> Imported EventGenericInstance
eventToEventGenericInstance ev@(SolidityEvent e) = do
  (EventDataDecl decl) <- eventToDataDecl ev
  let capConst = capitalize decl.constructor
  import' "Data.Generic.Rep.Eq" [IVal "genericEq"]
  import' "Data.Generic.Rep.Show" [IVal "genericShow"]
  import' "Data.Generic.Rep" [IClass "Generic"]
  pure $
    EventGenericInstance
      { instanceNames: (\n -> "eventGeneric" <> capConst <> n) <$> ["Show", "eq"]
      , instanceTypes: (\t -> t <> " " <> capConst) <$> ["Show", "Eq"]
      , genericDefs: ["show = genericShow", "eq = genericEq"]
      , genericDeriving: "derive instance generic" <> capConst <> " :: Generic " <> capConst <> " _"
      }

data EventDecodeInstance =
  EventDecodeInstance { indexedTuple :: String
                      , nonIndexedTuple :: String
                      , combinedType :: String
                      , anonymous :: Boolean
                      }

instance codeEventDecodeInstance :: Code EventDecodeInstance where
  genCode (EventDecodeInstance ev) _ = do
    import' "Network.Ethereum.Web3.Solidity" [IClass "IndexedEvent"]
    let indexedEventDecl = "instance indexedEvent" <> ev.combinedType <> " :: IndexedEvent " <> ev.indexedTuple <> " " <> ev.nonIndexedTuple <> " " <> ev.combinedType <> " where"
        indexedEventBody = "isAnonymous _ = " <> show ev.anonymous
    pure $
      newLine1
        [ indexedEventDecl
        , "  " <> indexedEventBody
        ]

eventToDecodeEventInstance :: SolidityEvent -> Imported EventDecodeInstance
eventToDecodeEventInstance event@(SolidityEvent ev) = do
  (EventDataDecl decl) <- eventToDataDecl event
  let 
    indexedTupleType = "Tuple" <> show (length decl.indexedTypes)
    nonIndexedTupleType = "Tuple" <> show (length decl.nonIndexedTypes)
    indexedTuple = "(" <> indexedTupleType <> " " <> joinWith " " (map taggedFactor decl.indexedTypes) <> ")"
    nonIndexedTuple = "(" <> nonIndexedTupleType <> " " <> joinWith " " (map taggedFactor decl.nonIndexedTypes) <> ")"
  import' "Network.Ethereum.Web3.Solidity" [IType indexedTupleType, IType nonIndexedTupleType]
  pure $ EventDecodeInstance {indexedTuple, nonIndexedTuple, combinedType: decl.constructor, anonymous: ev.anonymous}
  where
  taggedFactor (Tuple label value) = "(Tagged (SProxy \"" <> label <> "\") " <> value <> ")"


data EventFilterInstance =
  EventFilterInstance { instanceName :: String
                      , instanceType :: String
                      , filterDef :: String
                      }

instance codeEventFilterInstance :: Code EventFilterInstance where
  genCode (EventFilterInstance i) _ = do
    import' "Network.Ethereum.Web3" [IClass "EventFilter"]
    let
      header = "instance " <> i.instanceName <> " :: EventFilter " <> i.instanceType <> " where"
      eventFilter = "\t" <> i.filterDef
    pure $ newLine1 [header, eventFilter]

eventId :: SolidityEvent -> HexString
eventId (SolidityEvent e) =
  let eventArgs = map (\a -> format a) e.inputs
  in sha3 $ e.name <> "(" <> joinWith "," eventArgs <> ")"

eventToEventFilterInstance :: SolidityEvent -> Imported EventFilterInstance
eventToEventFilterInstance ev@(SolidityEvent e) = do
  (EventDataDecl decl) <- eventToDataDecl ev
  filterExpr <- mkFilterExpr "addr"
  pure $
    EventFilterInstance
      { instanceName: "eventFilter" <> capitalize decl.constructor
      , instanceType: capitalize decl.constructor
      , filterDef: "eventFilter _ addr = " <> filterExpr
      }
  where
  mkFilterExpr :: String -> Imported String
  mkFilterExpr addr = do
    import' "Network.Ethereum.Web3.Types.Types" [ITypeCtr "HexString"]
    import' "Data.Maybe" [ITypeCtr "Maybe"]
    import' "Data.Lens" [IOp ".~"]
    import' "Network.Ethereum.Web3" [IVal "_address", IVal "_topics"]
    import' "Network.Ethereum.Web3.Types" [IVal "defaultFilter"]
    let 
      nIndexedArgs = length $ filter (\(IndexedSolidityValue v) -> v.indexed) e.inputs
      indexedVals = 
        if nIndexedArgs == 0
          then ""
          else "," <> joinWith "," (replicate nIndexedArgs "Nothing")
      eventIdStr = "Just (" <> "HexString " <> "\"" <> (unHex $ eventId ev) <> "\"" <> ")"
    pure $ 
      fold
        ["defaultFilter"
        , "\n\t\t"
        , joinWith "\n\t\t"
          [ "# _address .~ Just " <> addr
          , "# _topics .~ Just [" <> eventIdStr <> indexedVals <> "]"
          ]
        ]


eventToEventCodeBlock :: SolidityEvent -> Imported CodeBlock
eventToEventCodeBlock ev@(SolidityEvent e) = do
  eventDec <- eventToDataDecl ev
  eventFilterInstance <- eventToEventFilterInstance ev
  decodeEventInstance <- eventToDecodeEventInstance ev
  eventGenericInstance <- eventToEventGenericInstance ev
  pure $ EventCodeBlock eventDec eventFilterInstance decodeEventInstance eventGenericInstance

--------------------------------------------------------------------------------

mkComment :: Array String -> String
mkComment cs = let sep = (fromCharArray $ replicate 80 '-') <> "\n"
               in  sep <> (newLine1 $ map (\s -> "-- | " <> s) cs) <> "\n" <> sep

data CodeBlock =
    FunctionCodeBlock FunTypeDecl HelperFunction
  | EventCodeBlock EventDataDecl  EventFilterInstance EventDecodeInstance EventGenericInstance

funToFunctionCodeBlock :: SolidityFunction -> GeneratorOptions -> Imported CodeBlock
funToFunctionCodeBlock fun@(SolidityFunction f) opts = do
    typeDecl <- funToTypeDecl fun opts
    helperFunction <- if isUnCurried f
                        then funToHelperFunction' fun opts
                        else funToHelperFunction false fun opts
    pure $ FunctionCodeBlock typeDecl helperFunction
  where
    isUnCurried f' = all (\(FunctionInput fi) -> fi.name /= "") f'.inputs && not (null f'.inputs)

newLine1 :: Array String -> String
newLine1 = joinWith "\n"

newLine2 :: Array String -> String
newLine2 = joinWith "\n\n"

instance codeFunctionCodeBlock :: Code CodeBlock where
  genCode (FunctionCodeBlock decl@(FunTypeDecl d) helper) opts = do
    let header = mkComment [d.typeName]
    declCode <- genCode decl opts
    helperCode <- genCode helper opts
    pure $
      newLine2
        [ header
        , declCode
        , helperCode
        ]
  genCode (EventCodeBlock decl@(EventDataDecl d) filterInst eventInst genericInst) opts = do
    let header = mkComment [d.constructor]
    declCode <- genCode decl opts
    filterInstCode <- genCode filterInst opts
    eventInstCode <- genCode eventInst opts
    genericInstCode <- genCode genericInst opts
    pure $
      newLine2
        [ header
        , declCode
        , filterInstCode
        , eventInstCode
        , genericInstCode
        ]

instance codeAbi :: Code Abi where
  genCode (Abi abi) opts = do
    codes <- for abi case _ of
      AbiFunction f -> do
        functionCodeBlock <- funToFunctionCodeBlock f opts
        genCode functionCodeBlock opts
      AbiEvent e -> do
        eventCodeBlock <- eventToEventCodeBlock e
        genCode eventCodeBlock opts
      AbiConstructor _ ->
        -- Constructor is only called when the contract is deployed
        -- now currently the library doesn't handle contract deployments very well.
        pure ""
      AbiFallback _ ->
        -- Fallback is a function that gets called in case someone
        -- sends ether to the contract with no function specified
        -- so it's like, you would never call it on purpose, so we ignore it.
        pure ""
    pure $ newLine2 codes

--------------------------------------------------------------------------------
-- | Tools to read and write the files
--------------------------------------------------------------------------------

type GeneratorOptions = {jsonDir :: FilePath, pursDir :: FilePath, truffle :: Boolean, prefix :: String, indentationLevel :: Int}

data IsCtrInImports = CtrIsInImports | CtrIsNotInImports
type ModuleImportsAcc = { types :: Map ModuleName IsCtrInImports, imports :: Array String }

runImports :: Imports -> String
runImports = mergeImports >>> map runImport >>> newLine1 >>> ("import Prelude \n\n" <> _)
  where
    runImport :: Tuple ModuleName ModuleImports -> String
    runImport (Tuple mName mImports) = "import " <> mName <> " (" <> joinWith ", " (runModuleImports mImports) <> ")"
    runModuleImports :: ModuleImports -> Array String
    runModuleImports =
      runAcc <<< foldl f { types: mempty, imports: mempty }
      where
      runAcc :: ModuleImportsAcc -> Array String
      runAcc acc = sort $ nub $ append acc.imports $ (toAscUnfoldable acc.types) >>= resolveCtrImports
      resolveCtrImports :: Tuple String IsCtrInImports -> Array String
      resolveCtrImports (Tuple typeName isCtrInImports) = case isCtrInImports of
        CtrIsInImports -> []
        CtrIsNotInImports -> [typeName]
      f :: ModuleImportsAcc -> ModuleImport -> ModuleImportsAcc
      f acc = case _ of
        IType a ->
          if member a acc.types
            then acc
            else acc{ types = insert a CtrIsNotInImports acc.types}
        ITypeCtr a ->
          case lookup a acc.types of
            Nothing ->
              { types: insert a CtrIsInImports acc.types, imports: acc.imports <> [ a <> "(..)" ]}
            Just CtrIsInImports ->
              acc
            Just CtrIsNotInImports ->
              { types: insert a CtrIsInImports acc.types, imports: acc.imports <> [ a <> "(..)" ]}
        ITypeOp a ->
          acc {imports = acc.imports <> [ "type (" <> a <> ")" ]}
        IClass a ->
          acc {imports = acc.imports <> [ "class " <> a ]}
        IVal a ->
          acc {imports = acc.imports <> [ a ]}
        IOp a ->
          acc {imports = acc.imports <> [ "(" <> a <> ")" ]}

    -- NOTE this also sorts modules as we use toAscUnfoldable
    mergeImports :: Imports -> Imports
    mergeImports = fromFoldableWith append >>> toAscUnfoldable

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
    let (Tuple code accImports) = runWriter $ genCode abi opts
    writeTextFile UTF8 destFile $ genPSModuleStatement opts destFile <> "\n"
      <> if code == "" then "" else runImports accImports <> "\n" <> code

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
