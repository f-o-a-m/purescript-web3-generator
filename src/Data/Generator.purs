module Data.Generator where

import Prelude

import Data.AbiParser (Abi(..), AbiType(..), BasicSolidityType(..), IndexedSolidityValue(..), NamedSolidityType(..), SolidityConstructor(..), SolidityEvent(..), SolidityFunction(..), SolidityType(..), format)
import Data.Array (concat, filter, length, replicate, snoc, (:))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (un)
import Data.String (drop, joinWith, take, toLower, toUpper)
import Data.Traversable (all, for, for_)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.HexString (fromBuffer)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Web3.Types (HexString, unHex)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen as Gen
import Tidy.Codegen.Monad as TidyM

--------------------------------------------------------------------------------

type CodeOptions =
  { exprPrefix :: String
  }

class Code a m where
  genCode :: a -> CodeOptions -> TidyM.CodegenT Void m (Array (CST.Declaration Void))

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

type PSTypeOptions = { onlyTuples :: Boolean, withLeafWrappers :: Boolean }

defaultPSTypeOpts :: PSTypeOptions
defaultPSTypeOpts = { onlyTuples: false, withLeafWrappers: false }

basicToPSType
  :: forall m
   . Monad m
  => PSTypeOptions
  -> BasicSolidityType
  -> TidyM.CodegenT Void m (CST.Type Void)
basicToPSType opts a = case a of
  SolidityBool -> unsafePartial $ maybeWrap opts true do
    pure $ Gen.typeCtor "Boolean"
  SolidityAddress -> unsafePartial $ maybeWrap opts true do
    Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "Address")
  SolidityUint n -> unsafePartial $ maybeWrap opts true do
    uintN <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "UIntN")
    pure $ Gen.typeApp uintN [ Gen.typeInt n ]
  SolidityInt n -> unsafePartial $ maybeWrap opts true do
    intN <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "IntN")
    pure $ Gen.typeApp (Gen.typeCtor intN) [ Gen.typeInt n ]
  SolidityString -> unsafePartial $ maybeWrap opts true do
    pure $ Gen.typeCtor "String"
  SolidityBytesN n -> unsafePartial $ maybeWrap opts true do
    bytesN <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "BytesN")
    pure $ Gen.typeApp (Gen.typeCtor bytesN) [ Gen.typeInt n ]
  SolidityBytesD -> unsafePartial $ maybeWrap opts true $
    Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "ImmutableBuffer")
  tup@(SolidityTuple factors) -> unsafePartial $ maybeWrap opts (noRecordsAtOrBelow $ BasicType tup)
    case factors of
      [] -> Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "Tuple0")
      [ NamedSolidityType { name: Nothing, type: t } ] -> do
        _t <- toPSType opts t
        tuple1 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "Tuple1")
        pure $ Gen.typeApp tuple1 [ _t ]
      _ -> case for_ factors \(NamedSolidityType { name }) -> name of
        Nothing -> unsafeCrashWith "Names should have been proved by fallback method which uses coordinates"
        _ | opts.onlyTuples -> do
          let tupleType = "Tuple" <> show (length factors)
          tuple <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
          ts <- for factors \(NamedSolidityType { name: _name, type: t }) -> do
            let n = unsafePartial $ fromJust _name
            _pst <- toPSType opts t
            tagInput (Tuple n _pst)
          pure $ Gen.typeApp tuple ts
        _ -> do
          fs <- for factors $ \(NamedSolidityType { name: _name, type: t }) ->
            let
              n = unsafePartial $ fromJust _name
            in
              Tuple n <$> toPSType opts t
          pure $ Gen.typeRecord fs Nothing

toPSType
  :: forall m
   . Monad m
  => PSTypeOptions
  -> SolidityType
  -> TidyM.CodegenT Void m (CST.Type Void)
toPSType opts s = case s of
  BasicType b -> basicToPSType opts b
  v@(SolidityVector n a) -> unsafePartial $
    if (noRecordsAtOrBelow v) then maybeWrap opts true do
      let l = Gen.typeInt n
      vector <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Vector")
      x <- toPSType opts { withLeafWrappers = false } a
      pure $ Gen.typeApp vector [ l, x ]
    else do
      let l = Gen.typeInt n
      vector <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Vector")
      x <- toPSType opts a
      pure $ Gen.typeApp vector [ l, x ]
  arr@(SolidityArray a) -> unsafePartial $
    if noRecordsAtOrBelow arr then maybeWrap opts true do
      t <- toPSType opts { withLeafWrappers = false } a
      pure $ Gen.typeApp (Gen.typeCtor "Array") [ t ]
    else do
      t <- toPSType opts a
      pure $ Gen.typeApp (Gen.typeCtor "Array") [ t ]

-- thi is such a hack
maybeWrap
  :: forall m
   . Monad m
  => PSTypeOptions
  -> Boolean
  -> TidyM.CodegenT Void m (CST.Type Void)
  -> TidyM.CodegenT Void m (CST.Type Void)
maybeWrap opts noRecs t
  | opts.withLeafWrappers && noRecs = unsafePartial do
      _identity <- Gen.typeCtor <$> TidyM.importFrom "Data.Identity" (TidyM.importType "Identity")
      _t <- t
      pure $ Gen.typeApp _identity [ _t ]
  | otherwise = t

noRecordsAtOrBelow
  :: SolidityType
  -> Boolean
noRecordsAtOrBelow = case _ of
  BasicType (SolidityTuple factors) -> all (\(NamedSolidityType { name }) -> isNothing name) factors
  BasicType _ -> true
  SolidityVector _ a -> noRecordsAtOrBelow a
  SolidityArray a -> noRecordsAtOrBelow a

--------------------------------------------------------------------------------

data FunType = Call | Send | Constructor

type TypeSyn = { typeSynName :: String, typeSynType :: CST.Type Void }

data FunIO
  = FIONone
  | FIOUnNamedTuple { len :: Int, asTuples :: CST.Type Void, type :: CST.Type Void }
  | FIORecord { len :: Int, asTuples :: CST.Type Void, type :: CST.Type Void }

-- | Data declaration
data FunData =
  FunData
    { name :: String
    , signature :: String
    , funType :: FunType
    , inputType :: FunIO
    , inputTypeSyn :: TypeSyn
    , returnType :: FunIO
    , returnTypeSyn :: TypeSyn
    , solidityFunction :: SolidityFunction
    }

makeFunData
  :: forall m
   . Monad m
  => CodeOptions
  -> SolidityFunction
  -> TidyM.CodegenT Void m FunData
makeFunData { exprPrefix } fun@(SolidityFunction f) = unsafePartial $ do
  inputType <- mkFIO f.inputs
  inputTypeSyn <- do
    typeSynType <- funTypeSyn inputType
    tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
    s <-
      if f.isConstructor then do
        TidyM.importOpen "Prelude"
        pure $ Gen.typeCtor "Void"
      else pure $ Gen.typeString (toSignature fun)
    pure
      { typeSynName: "Fn" <> capitalize f.name <> "Input"
      , typeSynType: Gen.typeApp tagged
          [ s
          , typeSynType
          ]
      }
  returnType <- mkFIO f.outputs
  returnTypeSyn <- do
    typeSynType <- funTypeSyn returnType
    pure
      { typeSynName: "Fn" <> capitalize f.name <> "Output"
      , typeSynType
      }
  pure $
    FunData
      { name: exprPrefix <> lowerCase f.name
      , inputType
      , inputTypeSyn
      , funType: if f.constant then Call else if f.isConstructor then Constructor else Send
      , signature: toSignature fun
      , returnType
      , returnTypeSyn
      , solidityFunction: fun
      }
  where
  mkFIO :: Array NamedSolidityType -> TidyM.CodegenT Void m FunIO
  mkFIO args = case args of
    [] -> pure FIONone
    _ -> unsafePartial $
      if all (\(NamedSolidityType fi) -> isJust fi.name) args then do
        fields <- for args \(NamedSolidityType fi) -> do
          _pst <- toPSType defaultPSTypeOpts fi.type
          let n = unsafePartial $ fromJust fi.name
          pure $ Tuple n _pst
        factors <- for args \(NamedSolidityType fi) -> do
          _pst <- toPSType { onlyTuples: true, withLeafWrappers: true } fi.type
          let n = unsafePartial $ fromJust fi.name
          tagInput (Tuple n _pst)
        let tupleType = "Tuple" <> show (length args)
        tuple <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
        pure $ FIORecord { len: length args, asTuples: Gen.typeApp tuple factors, type: Gen.typeRecord fields Nothing }
      else do
        ts <- for args \(NamedSolidityType fi) -> toPSType defaultPSTypeOpts fi.type
        let tupleType = "Tuple" <> show (length args)
        tuple <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType tupleType)
        let asTuples = Gen.typeApp tuple ts
        pure case ts of
          [ t ] -> FIOUnNamedTuple { len: length args, asTuples, type: t }
          _ -> FIOUnNamedTuple { len: length ts, asTuples, type: asTuples }

  funTypeSyn :: FunIO -> TidyM.CodegenT Void m (CST.Type Void)
  funTypeSyn = case _ of
    FIONone -> unsafePartial $ Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "Tuple0")
    FIOUnNamedTuple t -> pure t.asTuples
    FIORecord t -> pure t.asTuples

tagInput
  :: forall m
   . Monad m
  => Tuple String (CST.Type Void)
  -> TidyM.CodegenT Void m (CST.Type Void)
tagInput (Tuple name _type) = unsafePartial $ do
  tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
  pure $ Gen.typeApp tagged [ Gen.typeString name, _type ]

mkFunction
  :: forall m
   . Monad m
  => FunData
  -> TidyM.CodegenT Void m (Array (CST.Declaration Void))
mkFunction fun@(FunData f) = unsafePartial do
  let SolidityFunction { payable } = f.solidityFunction
  _txOpts <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "TransactionOptions")
  payAmt <- Gen.typeCtor <$>
    if payable then TidyM.importFrom "Network.Ethereum.Web3.Types.TokenUnit" (TidyM.importType "MinorUnit")
    else TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "NoPay")
  let txOpts = Gen.typeApp _txOpts [ payAmt ]
  case f.funType of
    Call -> do
      sig <- mkCallSignature fun txOpts
      call <- mkCall fun
      pure $ [ sig, call ]
    Send -> do
      sig <- mkSendSignature fun txOpts
      call <- mkSend fun
      pure $ [ sig, call ]
    Constructor -> do
      sig <- mkConstructorSignature fun txOpts
      call <- mkConstructor fun
      pure $ [ sig, call ]

mkConstructor
  :: forall m
   . Monad m
  => FunData
  -> TidyM.CodegenT Void m (CST.Declaration Void)
mkConstructor (FunData f) = unsafePartial do
  let
    vars = [ "bytecode", "txOpts" ]
    x = "x"
    binderVars = (Gen.binderVar <$> vars) <> case f.inputType of
      FIONone -> []
      _ -> [ Gen.binderVar x ]
  tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
  sendArg <- case f.inputType of
    FIONone -> do
      tuple0 <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
      pure $ Gen.exprTyped (Gen.exprApp tagged [ tuple0 ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
    FIOUnNamedTuple { len } ->
      if len > 1 then
        pure $ Gen.exprTyped (Gen.exprApp tagged [ Gen.exprIdent x ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
      else do
        tuple1 <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple1" "Tuple1")
        pure $ Gen.exprTyped (Gen.exprApp tagged [ Gen.exprApp tuple1 [ Gen.exprIdent x ] ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
    FIORecord _ -> do
      fromRecord <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "fromRecord")
      pure $ Gen.exprTyped
        (Gen.exprApp tagged [ Gen.exprApp fromRecord [ Gen.exprIdent x ] ])
        (Gen.typeCtor f.inputTypeSyn.typeSynName)
  deployContract <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "deployContract")
  let sendExpr = Gen.exprApp deployContract ((Gen.exprIdent <$> vars) `snoc` sendArg)
  pure $ Gen.declValue f.name binderVars sendExpr

mkConstructorSignature
  :: forall m
   . Monad m
  => FunData
  -> CST.Type Void
  -> TidyM.CodegenT Void m (CST.Declaration Void)
mkConstructorSignature (FunData f) txOpts = unsafePartial do
  let
    inputType = case f.inputType of
      FIONone -> Nothing
      FIOUnNamedTuple { type: t } -> Just t
      FIORecord { type: t } -> Just t
  web3 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Web3")
  hxString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
  let
    returnType = Gen.typeApp web3 [ hxString ]
    args = [ txOpts, hxString ] <> maybe [] pure inputType
  pure $ Gen.declSignature f.name (Gen.typeArrow args returnType)

mkSendSignature
  :: forall m
   . Monad m
  => FunData
  -> CST.Type Void
  -> TidyM.CodegenT Void m (CST.Declaration Void)
mkSendSignature (FunData f) txOpts = unsafePartial do
  let
    inputType = case f.inputType of
      FIONone -> Nothing
      FIOUnNamedTuple { type: t } -> Just t
      FIORecord { type: t } -> Just t
  web3 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Web3")
  hxString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
  let
    returnType = Gen.typeApp web3 [ hxString ]
    args = [ txOpts ] <> maybe [] pure inputType
  pure $ Gen.declSignature f.name (Gen.typeArrow args returnType)

mkSend
  :: forall m
   . Monad m
  => FunData
  -> TidyM.CodegenT Void m (CST.Declaration Void)
mkSend (FunData f) = unsafePartial do
  let
    vars = [ "txOpts" ]
    x = "x"
    binderVars = (Gen.binderVar <$> vars) <> case f.inputType of
      FIONone -> []
      _ -> [ Gen.binderVar x ]
  tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
  sendArg <- case f.inputType of
    FIONone -> do
      tuple0 <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
      pure $ Gen.exprTyped (Gen.exprApp tagged [ tuple0 ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
    FIOUnNamedTuple { len } ->
      if len > 1 then
        pure $ Gen.exprTyped (Gen.exprApp tagged [ Gen.exprIdent x ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
      else do
        tuple1 <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple1" "Tuple1")
        pure $ Gen.exprTyped (Gen.exprApp tagged [ Gen.exprApp tuple1 [ Gen.exprIdent x ] ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
    FIORecord _ -> do
      fromRecord <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "fromRecord")
      pure $ Gen.exprTyped
        (Gen.exprApp tagged [ Gen.exprApp fromRecord [ Gen.exprIdent x ] ])
        (Gen.typeCtor f.inputTypeSyn.typeSynName)
  sendTx <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "sendTx")
  let sendExpr = Gen.exprApp sendTx ((Gen.exprIdent <$> vars) `snoc` sendArg)
  pure $ Gen.declValue f.name binderVars sendExpr

mkCallSignature
  :: forall m
   . Monad m
  => FunData
  -> CST.Type Void
  -> TidyM.CodegenT Void m (CST.Declaration Void)
mkCallSignature (FunData f) txOpts = unsafePartial do
  let
    inputType = case f.inputType of
      FIONone -> Nothing
      FIOUnNamedTuple { type: t } -> Just t
      FIORecord { type: t } -> Just t
  returnType <- do
    web3 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Web3")
    callError <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "CallError")
    _either <- Gen.typeCtor <$> TidyM.importFrom "Data.Either" (TidyM.importType "Either")
    let
      _t = case f.returnType of
        FIONone -> Gen.typeCtor "Unit"
        FIOUnNamedTuple { type: t } -> t
        FIORecord { type: t } -> t
    pure $ Gen.typeApp web3 [ Gen.typeApp _either [ callError, _t ] ]
  chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
  let args = [ txOpts, chainCursor ] <> maybe [] pure inputType
  pure $ Gen.declSignature f.name (Gen.typeArrow args returnType)

mkCall
  :: forall m
   . Monad m
  => FunData
  -> TidyM.CodegenT Void m (CST.Declaration Void)
mkCall (FunData f) = unsafePartial do
  let
    vars = [ "txOpts", "chainCursor" ]
    x = "x"
    binderVars = (Gen.binderVar <$> vars) <> case f.inputType of
      FIONone -> []
      _ -> [ Gen.binderVar x ]
  tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importValue "tagged")
  callArg <- case f.inputType of
    FIONone -> do
      tuple0 <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
      pure $ Gen.exprTyped (Gen.exprApp tagged [ tuple0 ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
    FIOUnNamedTuple { len } ->
      if len > 1 then
        pure $ Gen.exprTyped (Gen.exprApp tagged [ Gen.exprIdent x ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
      else do
        tuple1 <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple1" "Tuple1")
        pure $ Gen.exprTyped (Gen.exprApp tagged [ Gen.exprApp tuple1 [ Gen.exprIdent x ] ]) (Gen.typeCtor f.inputTypeSyn.typeSynName)
    FIORecord _ -> do
      fromRecord <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "fromRecord")
      pure $ Gen.exprTyped
        (Gen.exprApp tagged [ Gen.exprApp fromRecord [ Gen.exprIdent x ] ])
        (Gen.typeCtor f.inputTypeSyn.typeSynName)
  call <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "call")
  let callExpr = Gen.exprApp call ((Gen.exprIdent <$> vars) `snoc` callArg)
  expr <- case f.returnType of
    FIONone -> pure callExpr
    FIOUnNamedTuple { len } ->
      if len == 1 then do
        untuple <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "unTuple1")
        TidyM.importOpen "Prelude"
        pure $
          Gen.exprOp (Gen.exprApp (Gen.exprIdent "map") [ untuple ])
            [ Gen.binaryOp "<$>" callExpr
            ]
      else pure callExpr
    FIORecord _ -> do
      toRecord <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "toRecord")
      _either <- Gen.typeCtor <$> TidyM.importFrom "Data.Either" (TidyM.importType "Either")
      TidyM.importOpen "Prelude"
      pure $ Gen.exprDo
        [ Gen.doBind
            ( Gen.binderTyped (Gen.binderVar "eRes")
                (Gen.typeApp _either [ Gen.typeWildcard, Gen.typeCtor f.returnTypeSyn.typeSynName ])
            )
            callExpr
        ]
        (Gen.exprApp (Gen.exprIdent "pure") [ Gen.exprApp (Gen.exprIdent "map") [ toRecord, Gen.exprIdent "eRes" ] ])
  pure $ Gen.declValue f.name binderVars expr

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
    let NamedSolidityType { name, type: t } = sv.type
    t <- toPSType { onlyTuples: true, withLeafWrappers: true } t
    pure $ Tuple (fromMaybe "" name) t
  nonIndexedTypes <- for nis \(IndexedSolidityValue sv) -> do
    let NamedSolidityType { name, type: t } = sv.type
    t <- toPSType { onlyTuples: true, withLeafWrappers: true } t
    pure $ Tuple (fromMaybe "" name) t
  recordType <- for ev.inputs \(IndexedSolidityValue sv) -> do
    let NamedSolidityType { name, type: t } = sv.type
    t <- toPSType defaultPSTypeOpts t
    pure $ Tuple (fromMaybe "" name) t
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
                              ( Gen.exprOp (Gen.exprCtor _just)
                                  [ Gen.binaryOp "$" (Gen.exprIdent _unsafePartial)
                                  , Gen.binaryOp "$" (Gen.exprIdent _fromJust)
                                  , Gen.binaryOp "$"
                                      ( Gen.exprApp (Gen.exprIdent _mkHexString)
                                          [ Gen.exprString $ unHex $ eventId decl.solidityEvent ]
                                      )
                                  ]
                                  : replicate (length decl.indexedTypes) (Gen.exprCtor _nothing)
                              )
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
            }
        in
          codegenFunction f
      AbiFallback _ -> pure []
      Unknown -> pure []
    pure $ concat codes
    where
    codegenFunction f = do
      funData@(FunData { inputTypeSyn, returnTypeSyn }) <- makeFunData opts f
      let inputSyn = unsafePartial $ Gen.declType inputTypeSyn.typeSynName [] inputTypeSyn.typeSynType
      let returnSyn = unsafePartial $ Gen.declType returnTypeSyn.typeSynName [] returnTypeSyn.typeSynType
      fun <- mkFunction funData
      pure $ inputSyn : returnSyn : fun

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
    fromBuffer $ keccak256 $ e.name <> "(" <> joinWith "," eventArgs <> ")"
