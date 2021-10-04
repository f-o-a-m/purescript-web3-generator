module Data.Generator where

import Prelude

import Control.Monad.Writer (Writer, tell)
import Data.AbiParser (Abi(..), AbiType(..), FunctionInput(..), IndexedSolidityValue(..), SolidityEvent(..), SolidityFunction(..), SolidityConstructor(..), SolidityType(..), format)
import Data.Array (filter, length, mapWithIndex, null, replicate, uncons, unsnoc, snoc, zip, zipWith, (:))
import Data.Foldable (fold)
import Data.Identity (Identity(..))
import Data.List (uncons) as List
import Data.List.Types (NonEmptyList(..)) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Data.String (drop, joinWith, take, toLower, toUpper)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), uncurry)
import Network.Ethereum.Core.HexString (fromByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Web3.Types (HexString, unHex)
import Partial.Unsafe (unsafeCrashWith)

import Tidy.Codegen as Gen
import Tidy.Codegen.Monad as TidyM
import PureScript.CST.Types as CST
import Partial.Unsafe (unsafePartial)
import Data.Exists (Exists, mkExists)



--------------------------------------------------------------------------------

type CodeOptions =
  { exprPrefix :: String
  }

class Code a e m where
  genCode :: a -> CodeOptions -> TidyM.CodegenT e m (CST.Declaration e)

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

toPSType :: forall e m . Monad m => SolidityType -> TidyM.CodegenT e m (CST.Type e)
toPSType s = unsafePartial $ Gen.typeParens <$> case s of
    SolidityBool -> do
      pure $ Gen.typeCtor "Boolean"
    SolidityAddress ->
      Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "Address")
    SolidityUint n -> do
      uintN <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "UIntN")
      digits <- makeDigits n
      pure $ Gen.typeApp uintN [Gen.typeParens digits]
    SolidityInt n -> do
      intN <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "IntN")
      digits <- makeDigits n
      pure $ Gen.typeApp (Gen.typeCtor intN) [Gen.typeParens digits]
    SolidityString -> do
      pure $ Gen.typeCtor "String"
    SolidityBytesN n -> do
      bytesN <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "BytesN")
      digits <- makeDigits n
      pure $ Gen.typeApp (Gen.typeCtor bytesN) [Gen.typeParens digits]
    SolidityBytesD ->
      Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "ByteString")
    SolidityVector ns a -> expandVector ns a
    SolidityArray a -> do
      t <- toPSType a
      pure $ Gen.typeApp (Gen.typeCtor "Array") [t]
  where
  makeDigits :: Int -> TidyM.CodegenT e m (CST.Type e)
  makeDigits n = do
    let digits :: Array String
        digits = map singleton <<< toCharArray <<< show $ n
    makeDigits' digits
    where 
      makeDigits' digits = unsafePartial $ 
        case unsnoc digits of
          Nothing -> unsafeCrashWith "impossible case reached in makeDigits"
          Just { init, last } -> do
            let mkD d = "D" <> d
            lastD <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ mkD last)
            done <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "DOne")
            let dType = Gen.typeApp (Gen.typeCtor done) [Gen.typeCtor lastD]
            case uncons init of
              Nothing -> pure dType
              Just {head, tail} -> do
                tailDs <- for tail $ \d -> do
                  d' <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ mkD d)
                  pure $ Gen.typeCtor d'
                digitAnd <- TidyM.importFrom "Network.Ethereum.Web3.Solidity.Size" (TidyM.importTypeOp ":&")
                let allRestDigits = map (Gen.binaryOp digitAnd) tailDs
                pure $ Gen.typeOp (Gen.typeCtor head) allRestDigits
      
  expandVector (List.NonEmptyList (n :| ns)) a' = unsafePartial do
      l <- makeDigits n
      vector <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Vector")
      case List.uncons ns of
        Nothing -> do
          x <- toPSType a'
          pure $ Gen.typeApp vector [Gen.typeParens l, x]
        Just {head, tail} -> do
          x <- expandVector (List.NonEmptyList $ head :| tail) a'
          pure $ Gen.typeApp vector [Gen.typeParens l, Gen.typeParens x]

tagInput
  :: forall e m. 
     Monad m
  => FunctionInput
  -> TidyM.CodegenT e m (CST.Type e)
tagInput (FunctionInput fi) = unsafePartial $ do
  ty <- toPSType fi.type
  tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
  proxy <- Gen.typeCtor <$> TidyM.importFrom "Type.Proxy" (TidyM.importType "Proxy")
  pure $ Gen.typeApp tagged 
    [ Gen.typeApp proxy [Gen.typeString fi.name]
    , ty
    ]

-- | Data declaration
data FunTypeDecl e =
  FunTypeDecl { signature :: String
              , factorType :: Array (CST.Type e)
              , typeName :: String
              }

funToTypeDecl 
  :: forall e m. 
     Monad m 
  => SolidityFunction 
  -> TidyM.CodegenT e m (Exists FunTypeDecl)
funToTypeDecl fun@(SolidityFunction f) = do
  factorType <- for f.inputs tagInput
  pure $ mkExists $
    FunTypeDecl
      { typeName: if isValidType f.name then capitalize $ f.name <> "Fn" else "FnT" <> f.name <> "Fn"
      , factorType
      , signature: toSignature fun
      }

toReturnType 
  :: forall e m. 
     Monad m
  => Boolean 
  -> Array SolidityType
  -> TidyM.CodegenT e m (CST.Type e)
toReturnType constant outputs' = unsafePartial do
  web3 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "Web3")
  if not constant
    then do
      hexString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
      pure $ Gen.typeApp web3 [hexString]
    else do
      callError <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "CallError")
      _either <- Gen.typeCtor <$> TidyM.importFrom "Data.Either" (TidyM.importType "Either")
      outputs <- for outputs' toPSType
      out <- case uncons outputs of
        Nothing -> pure $ Gen.typeCtor "Unit"
        Just { head, tail: []} -> pure head
        Just _ -> do
          let tupleType = "Tuple" <> show (length outputs)
          tuple <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
          pure $ Gen.typeParens $ Gen.typeApp tuple outputs
      pure $ Gen.typeApp web3 
        [ Gen.typeParens $ Gen.typeApp _either
          [ callError
          , out
          ]
        ]

data EventDataDecl e =
  EventDataDecl { constructor :: String
                , indexedTypes :: Array (Tuple String (CST.Type e))
                , nonIndexedTypes :: Array (Tuple String (CST.Type e))
                , recordType :: Array (Tuple String (CST.Type e))
                }

eventToDataDecl 
  :: forall e m. 
     Monad m
  => SolidityEvent
  -> TidyM.CodegenT e m (Exists EventDataDecl)
eventToDataDecl (SolidityEvent ev) = mkExists <$> do
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
  pure $ EventDataDecl
    { constructor: if isValidType (capitalize ev.name) then capitalize ev.name else "EvT" <> ev.name
    , indexedTypes
    , nonIndexedTypes
    , recordType
    }
  
eventNewtypeDecl 
  :: forall e m. 
     Monad m
  => SolidityEvent
  -> EventDataDecl e
  -> TidyM.CodegenT e m (Array (CST.Declaration e))
eventNewtypeDecl e (EventDataDecl decl) = unsafePartial do
  let typeDecl = Gen.declNewtype decl.constructor [] decl.constructor (Gen.typeRecord decl.recordType Nothing) 
  newtypeDecl <- do 
    _newtype <- TidyM.importFrom "Data.Newtype" (TidyM.importClass "Newtype")
    pure $ Gen.declDerive Nothing [] _newtype [Gen.typeCtor decl.constructor, Gen.typeWildcard]
  genericDecl <- do 
    _generic <- TidyM.importFrom "Data.Generic.Rep" (TidyM.importClass "Generic")
    pure $ Gen.declDerive Nothing [] _generic [Gen.typeCtor decl.constructor, Gen.typeWildcard]
  showDecl <- do 
    TidyM.importOpen "Prelude"
    _genericShow <- TidyM.importFrom "Data.Show.Generic" (TidyM.importValue "genericShow")
    pure $ 
      Gen.declInstance Nothing [] "Show" [Gen.typeCtor decl.constructor]
        [Gen.instValue "show" [] (Gen.exprIdent _genericShow)]
  eqDecl <- do 
    TidyM.importOpen "Prelude"
    _genericEq <- TidyM.importFrom "Data.Eq.Generic" (TidyM.importValue "genericEq")
    pure $ 
      Gen.declInstance Nothing [] "Eq" [Gen.typeCtor decl.constructor]
        [Gen.instValue "eq" [] (Gen.exprIdent _genericEq)]
  eventFilterDecl <- do
    TidyM.importOpen "Prelude"
    {_fromJust, _just, _nothing} <-
      TidyM.importFrom "Data.Maybe"
        { _fromJust: TidyM.importValue "fromJust"
        , _just: TidyM.importCtor "Maybe" "Just"
        , _nothing: TidyM.importCtor "Maybe" "Nothing"
        }
    _unsafePartial <- TidyM.importFrom "Unsafe.Partial" (TidyM.importValue "unsafePartial")
    set <- TidyM.importFrom "Data.Lens" (TidyM.importValue "set")
    {_defaultFilter, _mkHexString} <- 
      TidyM.importFrom "Newtork.Ethereum.Web3.Types"
        { _defaultFilter: TidyM.importValue "defaultFilter"
        , _mkHexString: TidyM.importValue "mkHexString"
        }
    {_address, _topics, eventFilterClass} <-
      TidyM.importFrom "Network.Ethereum.Web3"
         { _address: TidyM.importValue "_address"
         , _topics: TidyM.importValue "_topics"
         , eventFilterClass: TidyM.importClass "EventFilter"
         }
    pure $ 
      Gen.declInstance Nothing [] eventFilterClass [Gen.typeCtor decl.constructor]
        [ Gen.instValue "eventFilter" [Gen.binderWildcard, Gen.binderVar "addr"] $
            Gen.exprOp (Gen.exprIdent _defaultFilter)
                [ Gen.binaryOp "#"  
                    (Gen.exprApp (Gen.exprIdent set)
                      [ Gen.exprIdent _address
                      , Gen.exprParens $ Gen.exprApp (Gen.exprCtor _just) [Gen.exprIdent "addr"]
                      ]
                    )
                , Gen.binaryOp "#"  
                    (Gen.exprApp (Gen.exprIdent set)
                      [ Gen.exprIdent _topics
                      , Gen.exprApp (Gen.exprCtor _just)
                          [ Gen.exprArray 
                              [ Gen.exprOp (Gen.exprCtor _just)
                                  [ Gen.binaryOp "$" (Gen.exprIdent _unsafePartial)
                                  , Gen.binaryOp "$" (Gen.exprIdent _fromJust)
                                  , Gen.binaryOp "$" (Gen.exprApp (Gen.exprIdent _mkHexString) [Gen.exprString $ unHex $ eventId e])
                                  ]
                                
                              , Gen.exprCtor _nothing
                              , Gen.exprCtor _nothing
                              ]
                          ]
                      ]
                    )
                ]
        ]
  pure 
    [ typeDecl
    , newtypeDecl
    , genericDecl
    , showDecl
    , eqDecl
    , eventFilterDecl
    ]

{-
--------------------------------------------------------------------------------
-- | Data decleration, instances, and helpers
--------------------------------------------------------------------------------


instance codeDataDecl :: Code FunTypeDecl where
  genCode (FunTypeDecl decl) _ = do
    let
      nArgs = length decl.factorTypes
      tupleType = "Tuple" <> show nArgs
    TidyM.importFrom "Data.Functor.Tagged" (Gen.importType "Tagged")
    TidyM.importFrom "Type.Proxy" (Gen.importType "Proxy")
    TidyM.importFrom "Network.Ethereum.Web3.Solidity" (Gen.importType tupleType)
    pure $ 
      declType decl.typeName [] $
        Gen.typeApp 
          (Gen.typeApp 
            (Gen.typeCtor "Tagged") 
            (Gen.typeParens 
              (Gen.typeApp 
                (Gen.typCtor "Proxy") 
                (Gen.typeParens decl.signature) 
              )
            )
          )
          decl.factorType

--------------------------------------------------------------------------------
-- | Helper functions (asynchronous call/send)
--------------------------------------------------------------------------------

type CurriedHelperFunctionR =
  { signature :: Array String
  , unpackExpr :: { name :: String, stockArgs :: Array String, payloadArgs :: Array String }
  , payload :: String
  , transport :: String
  , constraints :: Array String
  , quantifiedVars :: Array String
  }

data HelperFunction
  = CurriedHelperFunction CurriedHelperFunctionR
  | UnCurriedHelperFunction
      { signature :: Array String
      , unpackExpr :: { name :: String, stockArgs :: Array String, stockArgsR :: Array String }
      , constraints :: Array String
      , quantifiedVars :: Array String
      , whereClause :: String
      }

funToHelperFunction :: Boolean -> SolidityFunction -> CodeOptions -> Imported CurriedHelperFunctionR
funToHelperFunction isWhereClause fun@(SolidityFunction f) opts = do
  (FunTypeDecl decl) <- funToTypeDecl fun
  import' "Network.Ethereum.Web3.Types" [IType "TransactionOptions"]
  sigPrefix <-
    if f.isConstructor
      then do
        import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
        import' "Network.Ethereum.Web3.Types" [IType "HexString"]
        pure ["TransactionOptions NoPay", "HexString"]
      else if f.constant
        then do
          import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
          import' "Network.Ethereum.Web3.Types" [IType "ChainCursor"]
          pure ["TransactionOptions NoPay", "ChainCursor"]
        else if f.payable
               then do
                   import' "Network.Ethereum.Web3.Types.TokenUnit" [IType "MinorUnit"]
                   pure ["TransactionOptions MinorUnit"]
               else do
                   import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
                   pure ["TransactionOptions NoPay"]
  let
    var = if isWhereClause then "y" else "x"
    constraints = []
    quantifiedVars = []
    stockVars =
      if f.isConstructor
         then [var <> "0", if isWhereClause then "bc'" else "bc"]
         else if f.constant
              then [var <> "0", if isWhereClause then "cm'" else "cm"]
              else [var <> "0"]
    offset = length stockVars
    inputs' = map (\(FunctionInput fi) -> fi.type) f.inputs
    conVars = mapWithIndex (\i _ -> var <> show (offset + i)) inputs'
  helperTransport <- toTransportPrefix f.isConstructor f.constant $ length f.outputs
  helperPayload <- toPayload decl.typeName conVars
  returnType <- toReturnType f.constant f.outputs
  ins <-
    if f.isUnCurried
      then for f.inputs tagInput
      else for f.inputs $ \(FunctionInput fi) -> toPSType fi.type
  pure
    { signature: sigPrefix <> ins <> [returnType]
    , unpackExpr:
        { name: lowerCase $ opts.exprPrefix <> f.name
        , stockArgs: stockVars
        , payloadArgs: conVars
        }
    , payload: helperPayload
    , transport: helperTransport
    , constraints: constraints
    , quantifiedVars: quantifiedVars
    }

funToHelperFunction' :: SolidityFunction -> CodeOptions -> Imported HelperFunction
funToHelperFunction' fun@(SolidityFunction f) opts = do
    import' "Network.Ethereum.Web3.Types" [IType "TransactionOptions"]
    sigPrefix <-
      if f.isConstructor
        then do
          import' "Network.Ethereum.Web3.Types" [IType "HexString"]
          import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
          pure ["TransactionOptions NoPay", "HexString"]
        else if f.constant
          then do
            import' "Network.Ethereum.Web3.Types" [IType "ChainCursor"]
            import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
            pure ["TransactionOptions NoPay", "ChainCursor"]
          else if f.payable
               then do
                 import' "Network.Ethereum.Web3.Types.TokenUnit" [IType "MinorUnit"]
                 pure ["TransactionOptions MinorUnit"]
               else do
                 import' "Network.Ethereum.Web3.Types" [IType "NoPay"]
                 pure ["TransactionOptions NoPay"]
    let
      constraints = []
      quantifiedVars = []
      stockVars = if f.isConstructor
                    then ["x0", "bc"]
                    else if f.constant
                      then ["x0", "cm"]
                      else ["x0"]
    returnType <- toReturnType f.constant f.outputs
    recIn <- recordInput f.inputs
    whereC <- whereHelper sigPrefix  returnType >>= \h -> genCode h opts {indentationLevel = opts.indentationLevel + 4}
    pure $
      UnCurriedHelperFunction
        { signature: sigPrefix <> [recIn, returnType]
        , unpackExpr:
            { name: lowerCase $ opts.exprPrefix <> f.name
            , stockArgs: stockVars <> ["r"]
            , stockArgsR: stockVars
            }
        , constraints: constraints
        , quantifiedVars: quantifiedVars
        , whereClause: whereC
        }
  where
    recordInput fis = do
      rowElems <- for fis $ \(FunctionInput fi) -> do
        ty <- toPSType fi.type
        pure $ fi.name <> " :: " <> ty
      pure $ "{ " <> joinWith ", " rowElems <> " }"
    whereHelper pre ret = do
      helper <- funToHelperFunction true fun opts
      tys <-
        if f.isUnCurried
          then for f.inputs tagInput
          else for f.inputs $ \(FunctionInput fi) -> toPSType fi.type
      pure $ CurriedHelperFunction helper
        { constraints = []
        , quantifiedVars = []
        , unpackExpr = helper.unpackExpr {name = helper.unpackExpr.name <> "'"}
        , signature = pre <> tys <> [ret]
        }

toTransportPrefix :: forall e m. Monad m => Boolean -> Boolean -> Int -> TidyM.CodeGenT e m _
toTransportPrefix isConstructor isCall outputCount = do
  fun <- if isConstructor
    then do
      Gen. "Network.Ethereum.Web3" [IVal "deployContract"]
      pure "deployContract"
    else if isCall
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

toPayload :: String -> Array String -> Imported String
toPayload typeName args = do
  import' "Data.Functor.Tagged" [IVal "tagged"]
  let tupleType = "Tuple" <> show (length args)
  import' "Network.Ethereum.Web3.Solidity" [ITypeCtr tupleType]
  pure $ "((tagged $ " <> tupleType <> " " <> joinWith " " args <> ") :: " <> typeName <> ")"


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
        eventGenerics = (\d -> "  " <> d) <$> i.genericDefs
        instances = zipWith (\h g -> h <> "\n" <> g) headers eventGenerics
    in pure $ newLine2 $ i.genericDeriving : instances

eventToEventGenericInstance :: SolidityEvent -> Imported EventGenericInstance
eventToEventGenericInstance ev@(SolidityEvent _) = do
  (EventDataDecl decl) <- eventToDataDecl ev
  let capConst = capitalize decl.constructor
  import' "Data.Eq.Generic" [IVal "genericEq"]
  import' "Data.Show.Generic" [IVal "genericShow"]
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
  indexedTypesTagged <- for decl.indexedTypes taggedFactor
  nonIndexedTypesTagged <- for decl.nonIndexedTypes taggedFactor
  let
    indexedTupleType = "Tuple" <> show (length decl.indexedTypes)
    nonIndexedTupleType = "Tuple" <> show (length decl.nonIndexedTypes)
    indexedTuple = paren $ indexedTupleType <> " " <> joinWith " " indexedTypesTagged
    nonIndexedTuple = paren $ nonIndexedTupleType <> " " <> joinWith " " nonIndexedTypesTagged
  import' "Network.Ethereum.Web3.Solidity" [IType indexedTupleType, IType nonIndexedTupleType]
  pure $ EventDecodeInstance {indexedTuple, nonIndexedTuple, combinedType: decl.constructor, anonymous: ev.anonymous}
  where
  taggedFactor (Tuple label value) = do
    import' "Data.Functor.Tagged" [IType "Tagged"]
    import' "Type.Proxy" [IType "Proxy"]
    pure $ "(Tagged (Proxy \"" <> label <> "\") " <> value <> ")"


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
      eventFilter = "  " <> i.filterDef
    pure $ newLine1 [header, eventFilter]


eventToEventFilterInstance :: SolidityEvent -> Imported EventFilterInstance
eventToEventFilterInstance ev@(SolidityEvent e) = do
  (EventDataDecl decl) <- eventToDataDecl ev
  filterExpr <- mkFilterExpr "addr"
  pure $
    EventFilterInstance
      { instanceName: "eventFilter" <> capitalize decl.constructor
      , instanceType: decl.constructor
      , filterDef: "eventFilter _ addr = " <> filterExpr
      }
  where
  mkFilterExpr :: String -> Imported String
  mkFilterExpr addr = do
    import' "Network.Ethereum.Web3.Types" [IVal "mkHexString"]
    import' "Data.Maybe" [ITypeCtr "Maybe", IVal "fromJust"]
    import' "Data.Lens" [IOp ".~"]
    import' "Network.Ethereum.Web3" [IVal "_address", IVal "_topics"]
    import' "Network.Ethereum.Web3.Types" [IVal "defaultFilter"]
    import' "Partial.Unsafe" [IVal "unsafePartial"]
    let
      nIndexedArgs = length $ filter (\(IndexedSolidityValue v) -> v.indexed) e.inputs
      indexedVals =
        if nIndexedArgs == 0
          then ""
          else "," <> joinWith "," (replicate nIndexedArgs "Nothing")
      eventIdStr = "Just ( unsafePartial $ fromJust $ mkHexString " <> "\"" <> (unHex $ eventId ev) <> "\"" <> ")"
    pure $
      fold
        ["defaultFilter"
        , "\n    "
        , joinWith "\n    "
          [ "# _address .~ Just " <> addr
          , "# _topics .~ Just [" <> eventIdStr <> indexedVals <> "]"
          ]
        ]


eventToEventCodeBlock :: SolidityEvent -> Imported CodeBlock
eventToEventCodeBlock ev@(SolidityEvent _) = do
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

funToFunctionCodeBlock :: SolidityFunction -> CodeOptions -> Imported CodeBlock
funToFunctionCodeBlock fun@(SolidityFunction f) opts = do
  typeDecl <- funToTypeDecl fun
  helperFunction <- if f.isUnCurried
                      then funToHelperFunction' fun opts
                      else funToHelperFunction false fun opts <#> CurriedHelperFunction
  pure $ FunctionCodeBlock typeDecl helperFunction

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

instance codeAbi :: Code (Abi Identity) where
  genCode (Abi abi) opts = do
    codes <- for abi $ un Identity >>> case _ of
      AbiFunction f -> do
        functionCodeBlock <- funToFunctionCodeBlock f opts
        genCode functionCodeBlock opts
      AbiEvent e -> do
        eventCodeBlock <- eventToEventCodeBlock e
        genCode eventCodeBlock opts
      AbiConstructor (SolidityConstructor c) -> do
        let f = SolidityFunction { name : "constructor"
                                 , inputs : c.inputs
                                 , outputs : []
                                 , constant : false
                                 , payable : false
                                 , isConstructor : true
                                 , isUnCurried: c.isUnCurried
                                 }
        functionCodeBlock <- funToFunctionCodeBlock f opts
        genCode functionCodeBlock opts
      AbiFallback _ ->
        -- Fallback is a function that gets called in case someone
        -- sends ether to the contract with no function specified
        -- so it's like, you would never call it on purpose, so we ignore it.
        pure ""
    pure $ newLine2 codes

-}
--------------------------------------------------------------------------------

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

isValidType :: String -> Boolean
isValidType s =
  let startChar = take 1 s
  -- if the first character is the same when both lowercase and uppercase it cannot be a valid type name (e.g. underscores)
  in toUpper startChar /= toLower startChar

-- e.g. "transferFrom(address,address,uint256)"
toSignature :: SolidityFunction -> String
toSignature (SolidityFunction f) =
  let args = map (\i -> format i) f.inputs
  in f.name <> "(" <> joinWith "," args <> ")"


eventId :: SolidityEvent -> HexString
eventId (SolidityEvent e) =
  let eventArgs = map (\a -> format a) e.inputs
  in fromByteString $ keccak256 $ e.name <> "(" <> joinWith "," eventArgs <> ")"