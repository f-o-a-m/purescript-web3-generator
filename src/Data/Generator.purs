module Data.Generator where

import Prelude

import Data.AbiParser (Abi(..), AbiType(..), FunctionInput(..), IndexedSolidityValue(..), SolidityEvent(..), SolidityFunction(..), SolidityConstructor(..), SolidityType(..), format)
import Data.Array (filter, length, uncons, snoc, (:), concat, unsafeIndex, (..))
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (un)
import Data.String (drop, joinWith, take, toLower, toUpper)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Network.Ethereum.Core.HexString (fromByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Web3.Types (HexString, unHex)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen as Gen
import Tidy.Codegen.Monad as TidyM
import PureScript.CST.Types as CST

--------------------------------------------------------------------------------

type CodeOptions =
  { exprPrefix :: String
  }

class Code a m where
  genCode :: a -> CodeOptions -> TidyM.CodegenT Void m (Array (CST.Declaration Void))

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

toPSType :: forall e m. Monad m => SolidityType -> TidyM.CodegenT e m (CST.Type e)
toPSType s = unsafePartial case s of
  SolidityBool -> do
    pure $ Gen.typeCtor "Boolean"
  SolidityAddress ->
    Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "Address")
  SolidityUint n -> do
    uintN <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "UIntN")
    pure $ Gen.typeApp uintN [ Gen.typeInt n ]
  SolidityInt n -> do
    intN <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "IntN")
    pure $ Gen.typeApp (Gen.typeCtor intN) [ Gen.typeInt n ]
  SolidityString -> do
    pure $ Gen.typeCtor "String"
  SolidityBytesN n -> do
    bytesN <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "BytesN")
    pure $ Gen.typeApp (Gen.typeCtor bytesN) [ Gen.typeInt n ]
  SolidityBytesD ->
    Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "ByteString")
  SolidityVector n a -> mkVector n a
  SolidityArray a -> do
    t <- toPSType a
    pure $ Gen.typeApp (Gen.typeCtor "Array") [ t ]

  where
  mkVector n a = unsafePartial do
    let l = Gen.typeInt n
    vector <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Vector")
    x <- toPSType a
    pure $ Gen.typeApp vector [ l, x ]

--------------------------------------------------------------------------------

-- | Data declaration
data FunData =
  FunData
    { signature :: String
    , factorTypes :: Array (Tuple String (CST.Type Void))
    , returnType :: CST.Type Void
    , typeName :: String
    , name :: String
    , solidityFunction :: SolidityFunction
    }

tagInput
  :: forall m
   . Monad m
  => Tuple String (CST.Type Void)
  -> TidyM.CodegenT Void m (CST.Type Void)
tagInput (Tuple name _type) = unsafePartial $ do
  tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
  pure $ Gen.typeApp tagged [ Gen.typeString name, _type ]

makeFunData
  :: forall m
   . Monad m
  => CodeOptions
  -> SolidityFunction
  -> TidyM.CodegenT Void m FunData
makeFunData { exprPrefix } fun@(SolidityFunction f) = do
  factorTypes <- for f.inputs $ \(FunctionInput fi) -> do
    ty <- toPSType fi.type
    pure $ Tuple fi.name ty
  returnType <- mkReturnType
  pure $
    FunData
      { typeName: if isValidType f.name then capitalize $ f.name <> "Fn" else "FnT" <> f.name <> "Fn"
      , factorTypes
      , signature: toSignature fun
      , returnType
      , name: exprPrefix <> lowerCase f.name
      , solidityFunction: fun
      }
  where

  mkReturnType = unsafePartial do
    web3 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "Web3")
    if not f.constant then do
      hexString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
      pure $ Gen.typeApp web3 [ hexString ]
    else do
      callError <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "CallError")
      _either <- Gen.typeCtor <$> TidyM.importFrom "Data.Either" (TidyM.importType "Either")
      outputs <- for f.outputs toPSType
      out <- case uncons outputs of
        Nothing -> pure $ Gen.typeCtor "Unit"
        Just { head, tail: [] } -> pure head
        Just _ -> do
          let tupleType = "Tuple" <> show (length outputs)
          tuple <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
          pure $ Gen.typeApp tuple outputs
      pure $ Gen.typeApp web3
        [ Gen.typeApp _either
            [ callError
            , out
            ]
        ]

funTypeSyn :: forall m. Monad m => FunData -> TidyM.CodegenT Void m (CST.Declaration Void)
funTypeSyn (FunData decl) = unsafePartial do
  let
    nArgs = length decl.factorTypes
    SolidityFunction { isUnCurried, isConstructor } = decl.solidityFunction
  tupleType <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ "Tuple" <> show nArgs)
  ts <- for decl.factorTypes $ \factor ->
    if isUnCurried then tagInput factor
    else pure $ snd factor
  tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
  t <-
    if isConstructor then do
      TidyM.importOpen "Prelude"
      pure $ Gen.typeCtor "Void"
    else pure $ Gen.typeString decl.signature
  pure
    $ Gen.declType decl.typeName []
    $ Gen.typeApp tagged
        [ t
        , Gen.typeApp tupleType ts
        ]

mkFunction
  :: forall m
   . Monad m
  => FunData
  -> TidyM.CodegenT Void m (Array (CST.Declaration Void))
mkFunction fun@(FunData f) = unsafePartial do
  let SolidityFunction { payable, constant, isConstructor } = f.solidityFunction
  _txOpts <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "TransactionOptions")
  payAmt <- Gen.typeCtor <$>
    if payable then TidyM.importFrom "Network.Ethereum.Web3.Types.TokenUnit" (TidyM.importType "MinorUnit")
    else TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "NoPay")
  let txOpts = Gen.typeApp _txOpts [ payAmt ]
  case uncons f.factorTypes of
    Nothing ->
      if constant then mkNoArgsCall txOpts
      else if isConstructor then do
        hexString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
        mkNoArgsSend [ txOpts, hexString ]
      else mkNoArgsSend [ txOpts ]
    Just { head, tail } -> do
      if constant then mkArgsCall txOpts { firstFactor: head, restFactors: tail }
      else if isConstructor then do
        hexString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
        mkArgsSend [ txOpts, hexString ] { firstFactor: head, restFactors: tail }
      else mkArgsSend [ txOpts ] { firstFactor: head, restFactors: tail }
  where

  mkNoArgsSend args = unsafePartial do
    let
      sig = Gen.declSignature f.name (Gen.typeArrow args f.returnType)
      SolidityFunction { isConstructor } = f.solidityFunction
      vars = map (\i -> "x" <> show i) (1 .. length args)
    expr <- do
      sendTx <-
        if isConstructor then Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "deployContract")
        else Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "sendTx")
      tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
      tupleC <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
      pure $
        Gen.exprApp sendTx
          ( map Gen.exprIdent vars `snoc`
              Gen.exprTyped (Gen.exprApp tagged [ tupleC ]) (Gen.typeCtor f.typeName)
          )
    pure [ sig, Gen.declValue f.name (map Gen.binderVar vars) expr ]

  mkArgsSend args factors = unsafePartial do
    let SolidityFunction { isUnCurried } = f.solidityFunction
    { sig, expr, vars } <-
      if isUnCurried then do
        let
          sig =
            let
              ts = args `snoc` Gen.typeRecord f.factorTypes Nothing
            in
              Gen.declSignature f.name $ Gen.typeArrow ts f.returnType
          vars = map (\i -> "x" <> show i) (1 .. (length args + 1))
        expr <- do
          uncurryFields <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Contract.Internal" (TidyM.importValue "uncurryFields")
          let
            helperName = f.name <> "'"
            idents = map Gen.exprIdent vars

          TidyM.importOpen "Prelude"
          pure $
            Gen.exprOp (Gen.exprApp uncurryFields [ unsafePartial $ fromJust $ Array.last idents ])
              [ Gen.binaryOp "$" (Gen.exprApp (Gen.exprIdent helperName) (Array.take (length args) idents))
              ]
        pure { sig, expr, vars }
      else do
        let
          sig =
            let
              ts = args <> map snd f.factorTypes
            in
              Gen.declSignature f.name $ Gen.typeArrow ts f.returnType
          vars = map (\i -> "x" <> show i) (1 .. (length f.factorTypes + length args))
        expr <- do
          let
            helperName = f.name <> "'"
            idents = map Gen.exprIdent vars
          pure $ Gen.exprApp (Gen.exprIdent helperName) idents
        pure { sig, expr, vars }
    helper <- mkHelperFunction fun factors
    pure
      [ sig
      , Gen.declValue f.name (Gen.binderVar <$> vars)
          (Gen.exprWhere expr helper)
      ]

  mkNoArgsCall txOpts = unsafePartial do
    chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
    let
      sig = Gen.declSignature f.name (Gen.typeArrow (txOpts : [ chainCursor ]) f.returnType)
      vars = map (\i -> "x" <> show i) [ 1, 2 ]
    expr <- do
      call <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "call")
      tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
      tupleC <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
      let
        idents = map Gen.exprIdent vars
        SolidityFunction { outputs } = f.solidityFunction
        callExpr =
          Gen.exprApp call
            [ unsafePartial $ unsafeIndex idents 0
            , unsafePartial $ unsafeIndex idents 1
            , Gen.exprTyped (Gen.exprApp tagged [ tupleC ]) (Gen.typeCtor f.typeName)
            ]
      if length outputs == 1 then do
        untuple <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "unTuple1")
        TidyM.importOpen "Prelude"
        pure $
          Gen.exprOp (Gen.exprApp (Gen.exprIdent "map") [ untuple ])
            [ Gen.binaryOp "<$>" callExpr
            ]
      else pure callExpr
    pure
      [ sig
      , Gen.declValue f.name (map Gen.binderVar vars) expr
      ]

  mkArgsCall txOpts factors = unsafePartial do
    chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
    let SolidityFunction { isUnCurried } = f.solidityFunction
    { expr, sig, vars } <-
      if isUnCurried then do
        let
          sig =
            let
              ts = txOpts : chainCursor : [ Gen.typeRecord f.factorTypes Nothing ]
            in
              Gen.declSignature f.name (Gen.typeArrow ts f.returnType)
          vars = map (\i -> "x" <> show i) [ 1, 2, 3 ]
        expr <- do
          uncurryFields <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Contract.Internal" (TidyM.importValue "uncurryFields")
          let helperName = f.name <> "'"
          TidyM.importOpen "Prelude"
          pure $
            Gen.exprOp (Gen.exprApp uncurryFields [ Gen.exprIdent $ unsafePartial $ unsafeIndex vars 2 ])
              [ Gen.binaryOp "$"
                  ( Gen.exprApp (Gen.exprIdent helperName)
                      [ Gen.exprIdent $ unsafePartial $ unsafeIndex vars 0
                      , Gen.exprIdent $ unsafePartial $ unsafeIndex vars 1
                      ]
                  )
              ]
        pure { sig, expr, vars }
      else do
        let
          sig =
            let
              ts = txOpts : chainCursor : map snd f.factorTypes
            in
              Gen.declSignature f.name (Gen.typeArrow ts f.returnType)
          vars = map (\i -> "x" <> show i) (1 .. (length f.factorTypes + 2))
        expr <- do
          let
            helperName = f.name <> "'"
            idents = map Gen.exprIdent vars
          pure $ Gen.exprApp (Gen.exprIdent helperName) idents
        pure { sig, expr, vars }
    helper <- mkHelperFunction fun factors
    pure
      [ sig
      , Gen.declValue f.name (map Gen.binderVar vars)
          ( Gen.exprWhere expr helper
          )
      ]

mkHelperFunction
  :: forall m
   . Monad m
  => FunData
  -> { firstFactor :: Tuple String (CST.Type Void)
     , restFactors :: Array (Tuple String (CST.Type Void))
     }
  -> TidyM.CodegenT Void m (Array (CST.LetBinding Void))
mkHelperFunction (FunData f) { firstFactor, restFactors } = unsafePartial do
  let SolidityFunction { constant, payable, isConstructor } = f.solidityFunction
  _txOpts <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "TransactionOptions")
  payAmt <- Gen.typeCtor <$>
    if payable then TidyM.importFrom "Network.Ethereum.Web3.Types.TokenUnit" (TidyM.importType "MinorUnit")
    else TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "NoPay")
  let
    txOpts = Gen.typeApp _txOpts [ payAmt ]
    helperName = f.name <> "'"
  if constant then mkCall helperName txOpts
  else if isConstructor then do
    hexString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
    mkSend helperName [ txOpts, hexString ]
  else mkSend helperName [ txOpts ]
  where
  -- NOTE: there is a lot of c/p between mkSend and mkCall but they ways in which they are different is just
  -- flat out annoying
  mkSend helperName args = unsafePartial do
    let vars = map (\i -> "_x" <> show i) (1 .. (length f.factorTypes + length args))
    expr <- do
      let SolidityFunction { isConstructor } = f.solidityFunction
      sendTx <-
        if isConstructor then Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "deployContract")
        else Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "sendTx")
      tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
      tupleC <- do
        let tupleType = "Tuple" <> show (length f.factorTypes)
        Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor tupleType tupleType)
      let idents = map Gen.exprIdent vars
      TidyM.importOpen "Prelude"
      pure $ Gen.exprApp sendTx
        ( Array.take (length args) idents `snoc`
            Gen.exprTyped
              ( Gen.exprOp tagged
                  [ Gen.binaryOp "$" $ Gen.exprApp tupleC (Array.drop (length args) idents)
                  ]
              )
              (Gen.typeCtor f.typeName)
        )

    let SolidityFunction { isUnCurried } = f.solidityFunction
    firstFactor' <-
      if isUnCurried then tagInput firstFactor
      else pure $ snd firstFactor
    restFactors' <- for restFactors $ \factor ->
      if isUnCurried then tagInput factor
      else pure $ snd factor
    pure
      [ Gen.letSignature helperName $ Gen.typeArrow (args <> (firstFactor' : restFactors')) f.returnType
      , Gen.letValue helperName (map Gen.binderVar vars) expr
      ]

  mkCall helperName txOpts = unsafePartial do
    chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
    -- the 3 comes from TxOpts -> ChainCursor -> firstFactor -> restFactors ->  returnType
    let vars = map (\i -> "_x" <> show i) (1 .. (length f.factorTypes + 2))
    expr <- do
      call <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "call")
      tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
      tupleC <- do
        let tupleType = "Tuple" <> show (length f.factorTypes)
        Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor tupleType tupleType)
      TidyM.importOpen "Prelude"
      let
        idents = map Gen.exprIdent vars
        SolidityFunction { outputs } = f.solidityFunction
        callExpr = Gen.exprApp call
          [ unsafePartial $ unsafeIndex idents 0
          , unsafePartial $ unsafeIndex idents 1
          , Gen.exprTyped
              ( Gen.exprOp tagged
                  [ Gen.binaryOp "$" $ Gen.exprApp tupleC (Array.drop 2 idents)
                  ]
              )
              (Gen.typeCtor f.typeName)
          ]
      if length outputs == 1 then do
        untuple <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "unTuple1")
        pure $
          Gen.exprOp (Gen.exprApp (Gen.exprIdent "map") [ untuple ])
            [ Gen.binaryOp "<$>" callExpr
            ]
      else pure callExpr
    let SolidityFunction { isUnCurried } = f.solidityFunction
    firstFactor' <-
      if isUnCurried then tagInput firstFactor
      else pure $ snd firstFactor
    restFactors' <- for restFactors $ \factor ->
      if isUnCurried then tagInput factor
      else pure $ snd factor
    pure
      [ Gen.letSignature helperName $ Gen.typeArrow (txOpts : chainCursor : firstFactor' : restFactors') f.returnType
      , Gen.letValue helperName (map Gen.binderVar vars) expr
      ]

--------------------------------------------------------------------------------

data EventData =
  EventData
    { constructor :: String
    , indexedTypes :: Array (Tuple String (CST.Type Void))
    , nonIndexedTypes :: Array (Tuple String (CST.Type Void))
    , recordType :: Array (Tuple String (CST.Type Void))
    , solidityEvent :: SolidityEvent
    }

mkEventData
  :: forall m
   . Monad m
  => SolidityEvent
  -> TidyM.CodegenT Void m EventData
mkEventData e@(SolidityEvent ev) = do
  let
    is = filter (\(IndexedSolidityValue sv) -> sv.indexed) ev.inputs
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
  pure $ EventData
    { constructor: if isValidType (capitalize ev.name) then capitalize ev.name else "EvT" <> ev.name
    , indexedTypes
    , nonIndexedTypes
    , recordType
    , solidityEvent: e
    }

eventDecls
  :: forall m
   . Monad m
  => EventData
  -> TidyM.CodegenT Void m (Array (CST.Declaration Void))
eventDecls (EventData decl) = unsafePartial do
  let typeDecl = Gen.declNewtype decl.constructor [] decl.constructor (Gen.typeRecord decl.recordType Nothing)

  newtypeDecl <- do
    _newtype <- TidyM.importFrom "Data.Newtype" (TidyM.importClass "Newtype")
    pure $ Gen.declDerive Nothing [] _newtype [ Gen.typeCtor decl.constructor, Gen.typeWildcard ]

  genericDecl <- do
    _generic <- TidyM.importFrom "Data.Generic.Rep" (TidyM.importClass "Generic")
    pure $ Gen.declDerive Nothing [] _generic [ Gen.typeCtor decl.constructor, Gen.typeWildcard ]

  showDecl <- do
    TidyM.importOpen "Prelude"
    _genericShow <- TidyM.importFrom "Data.Show.Generic" (TidyM.importValue "genericShow")
    pure $
      Gen.declInstance Nothing [] "Show" [ Gen.typeCtor decl.constructor ]
        [ Gen.instValue "show" [] (Gen.exprIdent _genericShow) ]

  eqDecl <- do
    TidyM.importOpen "Prelude"
    _genericEq <- TidyM.importFrom "Data.Eq.Generic" (TidyM.importValue "genericEq")
    pure $
      Gen.declInstance Nothing [] "Eq" [ Gen.typeCtor decl.constructor ]
        [ Gen.instValue "eq" [] (Gen.exprIdent _genericEq) ]

  indexedEventDecl <- do
    let
      f (Tuple name ty) = do
        tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
        pure $ Gen.typeApp tagged
          [ Gen.typeString name
          , ty
          ]
    indexedTypesTagged <- for decl.indexedTypes f
    nonIndexedTypesTagged <- for decl.nonIndexedTypes f
    indexedTuple <- do
      let tupleType = "Tuple" <> show (length decl.indexedTypes)
      Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
    nonIndexedTuple <- do
      let tupleType = "Tuple" <> show (length decl.nonIndexedTypes)
      Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
    indexedEventClass <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importClass "IndexedEvent")
    let SolidityEvent { anonymous } = decl.solidityEvent
    pure $
      Gen.declInstance Nothing [] indexedEventClass
        [ Gen.typeApp indexedTuple $ indexedTypesTagged
        , Gen.typeApp nonIndexedTuple $ nonIndexedTypesTagged
        , Gen.typeCtor decl.constructor
        ]
        [ Gen.instValue "isAnonymous" [ Gen.binderWildcard ] (Gen.exprBool anonymous)
        ]

  eventFilterDecl <- do
    TidyM.importOpen "Prelude"
    { _fromJust, _just, _nothing } <-
      TidyM.importFrom "Data.Maybe"
        { _fromJust: TidyM.importValue "fromJust"
        , _just: TidyM.importCtor "Maybe" "Just"
        , _nothing: TidyM.importCtor "Maybe" "Nothing"
        }
    _unsafePartial <- TidyM.importFrom "Partial.Unsafe" (TidyM.importValue "unsafePartial")
    set <- TidyM.importFrom "Data.Lens" (TidyM.importValue "set")
    { _defaultFilter, _mkHexString } <-
      TidyM.importFrom "Network.Ethereum.Web3.Types"
        { _defaultFilter: TidyM.importValue "defaultFilter"
        , _mkHexString: TidyM.importValue "mkHexString"
        }
    { _address, _topics, eventFilterClass } <-
      TidyM.importFrom "Network.Ethereum.Web3"
        { _address: TidyM.importValue "_address"
        , _topics: TidyM.importValue "_topics"
        , eventFilterClass: TidyM.importClass "EventFilter"
        }
    pure $
      Gen.declInstance Nothing [] eventFilterClass [ Gen.typeCtor decl.constructor ]
        [ Gen.instValue "eventFilter" [ Gen.binderWildcard, Gen.binderVar "addr" ] $
            Gen.exprOp (Gen.exprIdent _defaultFilter)
              [ Gen.binaryOp "#"
                  ( Gen.exprApp (Gen.exprIdent set)
                      [ Gen.exprIdent _address
                      , Gen.exprApp (Gen.exprCtor _just) [ Gen.exprIdent "addr" ]
                      ]
                  )
              , Gen.binaryOp "#"
                  ( Gen.exprApp (Gen.exprIdent set)
                      [ Gen.exprIdent _topics
                      , Gen.exprApp (Gen.exprCtor _just)
                          [ Gen.exprArray
                              [ Gen.exprOp (Gen.exprCtor _just)
                                  [ Gen.binaryOp "$" (Gen.exprIdent _unsafePartial)
                                  , Gen.binaryOp "$" (Gen.exprIdent _fromJust)
                                  , Gen.binaryOp "$"
                                      ( Gen.exprApp (Gen.exprIdent _mkHexString)
                                          [ Gen.exprString $ unHex $ eventId decl.solidityEvent ]
                                      )
                                  ]
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
    , indexedEventDecl
    ]

--------------------------------------------------------------------------------

instance Monad m => Code (Abi Identity) m where
  genCode (Abi abi) opts = do
    codes <- for abi $ un Identity >>> case _ of
      AbiFunction f -> codegenFunction f
      AbiEvent e -> mkEventData e >>= eventDecls
      AbiConstructor (SolidityConstructor c) ->
        let
          f = SolidityFunction
            { name: "constructor"
            , inputs: c.inputs
            , outputs: []
            , constant: false
            , payable: false
            , isConstructor: true
            , isUnCurried: c.isUnCurried
            }
        in
          codegenFunction f
      AbiFallback _ -> pure []
      Unknown -> pure []
    pure $ concat codes
    where
    codegenFunction f = do
      funData <- makeFunData opts f
      syn <- funTypeSyn funData
      fun <- mkFunction funData
      pure $ syn : fun

--------------------------------------------------------------------------------

capitalize :: String -> String
capitalize s =
  let
    h = toUpper $ take 1 s
    rest = drop 1 s
  in
    h <> rest

lowerCase :: String -> String
lowerCase s =
  let
    h = toLower $ take 1 s
    rest = drop 1 s
  in
    h <> rest

isValidType :: String -> Boolean
isValidType s =
  let
    startChar = take 1 s
  -- if the first character is the same when both lowercase and uppercase it cannot be a valid type name (e.g. underscores)
  in
    toUpper startChar /= toLower startChar

-- e.g. "transferFrom(address,address,uint256)"
toSignature :: SolidityFunction -> String
toSignature (SolidityFunction f) =
  let
    args = map (\i -> format i) f.inputs
  in
    f.name <> "(" <> joinWith "," args <> ")"

eventId :: SolidityEvent -> HexString
eventId (SolidityEvent e) =
  let
    eventArgs = map (\a -> format a) e.inputs
  in
    fromByteString $ keccak256 $ e.name <> "(" <> joinWith "," eventArgs <> ")"
