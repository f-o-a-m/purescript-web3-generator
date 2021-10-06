module Data.Generator where

import Prelude

import Control.Monad.Writer (Writer, tell)
import Data.AbiParser (Abi(..), AbiType(..), FunctionInput(..), IndexedSolidityValue(..), SolidityEvent(..), SolidityFunction(..), SolidityConstructor(..), SolidityType(..), format)
import Data.Array (filter, length, mapWithIndex, null, replicate, uncons, unsnoc, snoc, zip, zipWith, (:), concat, unsafeIndex, (..))
import Data.Array as Array
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



--------------------------------------------------------------------------------

type CodeOptions =
  { exprPrefix :: String
  }

class Code a m where
  genCode :: a -> CodeOptions -> TidyM.CodegenT Void m (Array (CST.Declaration Void))

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

toPSType :: forall e m . Monad m => SolidityType -> TidyM.CodegenT e m (CST.Type e)
toPSType s = unsafePartial case s of
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
      let t' = if typeIsMultipart a 
                 then Gen.typeParens t 
                 else t
      pure $ Gen.typeApp (Gen.typeCtor "Array") [t']
      
  where
  makeDigits :: Int -> TidyM.CodegenT e m (CST.Type e)
  makeDigits n = unsafePartial do
    let digits :: Array String
        digits = map singleton <<< toCharArray <<< show $ n
    
    case unsnoc digits of
      Nothing -> unsafeCrashWith "impossible case reached in makeDigits"
      Just { init, last } -> do
        let mkD d = "D" <> d
        lastD <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ mkD last)
        done <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType "DOne")
        let dType = Gen.typeApp (Gen.typeCtor done) [Gen.typeCtor lastD]
        case uncons init of
          Nothing -> pure dType
          Just {head, tail} -> Gen.typeParens <$> do
            tailDs <- for tail $ \d -> do
              d' <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ mkD d)
              pure $ Gen.typeCtor d'
            _ <- TidyM.importFrom "Network.Ethereum.Web3.Solidity.Size" (TidyM.importTypeOp "(:&)")
            head' <- TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ mkD head)
            let allRestDigits = map (Gen.binaryOp ":&") (snoc tailDs dType)
            pure $ Gen.typeOp (Gen.typeCtor head') allRestDigits
      
  expandVector (List.NonEmptyList (n :| ns)) a = unsafePartial do
      l <- makeDigits n
      vector <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importType "Vector")
      case List.uncons ns of
        Nothing -> do
          x <- toPSType a
          let x' = if typeIsMultipart a  
                     then Gen.typeParens x 
                     else x 
          pure $ Gen.typeApp vector [l, x]
        Just {head, tail} -> do
          x <- expandVector (List.NonEmptyList $ head :| tail) a
          pure $ Gen.typeApp vector [l, Gen.typeParens x]

--------------------------------------------------------------------------------

-- | Data declaration
data FunData =
  FunData { signature :: String
          , factorTypes :: Array (Tuple String (CST.Type Void))
          , returnType :: CST.Type Void
          , typeName :: String
          , name :: String
          , solidityFunction :: SolidityFunction
          }


tagInput
  :: forall m. 
     Monad m
  => Tuple String (CST.Type Void)
  -> TidyM.CodegenT Void m (CST.Type Void)
tagInput (Tuple name _type) = unsafePartial $ do
  tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
  proxy <- Gen.typeCtor <$> TidyM.importFrom "Type.Proxy" (TidyM.importType "Proxy")
  pure $ Gen.typeApp tagged 
    [ Gen.typeApp proxy [Gen.typeString name]
    , _type
    ]

makeFunData
  :: forall m. 
     Monad m 
  => CodeOptions
  -> SolidityFunction 
  -> TidyM.CodegenT Void m FunData
makeFunData {exprPrefix} fun@(SolidityFunction f) = do
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
      , name: exprPrefix <> f.name
      , solidityFunction: fun
      }
  where

    mkReturnType = unsafePartial do
      web3 <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "Web3")
      if not f.constant
        then do
          hexString <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "HexString")
          pure $ Gen.typeApp web3 [hexString]
        else do
          callError <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "CallError")
          _either <- Gen.typeCtor <$> TidyM.importFrom "Data.Either" (TidyM.importType "Either")
          outputs <- for f.outputs toPSType
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


funTypeSyn :: forall m. Monad m => FunData -> TidyM.CodegenT Void m (CST.Declaration Void)
funTypeSyn (FunData decl) = unsafePartial do
  let nArgs = length decl.factorTypes
  tupleType <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importType $ "Tuple" <> show nArgs)
  ts <- for decl.factorTypes tagInput
  pure $ 
    Gen.declType decl.typeName [] $ Gen.typeApp tupleType ts
        

{-
    transfer' :: TransactionOptions NoPay -> (Tagged (Proxy "to") Address) -> (Tagged (Proxy "amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: TransferFn)

  let 

-}

mkFunction
  :: forall m.
     Monad m
  => CodeOptions
  -> FunData
  -> TidyM.CodegenT Void m (Array (CST.Declaration Void))
mkFunction opts fun@(FunData f) = unsafePartial do
  let SolidityFunction{payable, constant} = f.solidityFunction
  _txOpts <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "TransactionOptions")
  payAmt <- Gen.typeCtor <$> 
    if payable 
      then TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "NoPay")
      else TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "MinorUnit")
  let txOpts = Gen.typeApp _txOpts [payAmt]
  case uncons f.factorTypes of
    Nothing -> 
      if constant 
        then mkNoArgsCall txOpts 
        else mkNoArgsSend txOpts
    Just {head, tail} -> do 
      helper <- mkHelperFunction opts fun {firstFactor: head, restFactors: tail}
      ds <- if constant then mkArgsCall txOpts else mkArgsSend txOpts
      pure $ ds <> helper
  where
  
    mkNoArgsSend txOpts = unsafePartial do
      let SolidityFunction{constant} = f.solidityFunction
          sig = Gen.declSignature f.name (Gen.typeOp txOpts [Gen.binaryOp "->" f.returnType])
      expr <- do
        sendTx <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "sendTx")
        tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Tagged" (TidyM.importValue "tagged")
        tupleC <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
        pure $ 
          Gen.exprApp sendTx
            [ Gen.exprIdent "x1"
            , Gen.exprParens $ Gen.exprTyped (Gen.exprParens $ Gen.exprApp tagged [tupleC]) (Gen.typeCtor f.typeName)
            ]
      pure $ [sig, Gen.declValue f.name [Gen.binderVar "x1"] expr]

    mkArgsSend txOpts = unsafePartial do
      let sig = let ts = Gen.typeRecord f.factorTypes Nothing : [f.returnType] 
                in Gen.declSignature f.name $ Gen.typeOp txOpts (map (Gen.binaryOp "->") ts)
      expr <- do
        uncurryFields <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Contract.Internal" (TidyM.importValue "uncurryFields")
        let helperName = f.name <> "'"
        pure $ 
          Gen.exprOp (Gen.exprApp uncurryFields [Gen.exprIdent "x1"])
            [ Gen.binaryOp "$" (Gen.exprApp (Gen.exprIdent helperName) [Gen.exprIdent "x2"])
            ]
      pure $ [sig, Gen.declValue f.name (Gen.binderVar <$> ["x1","x2"]) expr]

    mkNoArgsCall txOpts = unsafePartial do
      chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
      let sig = let ts = chainCursor : [f.returnType]
                in Gen.declSignature f.name (Gen.typeOp txOpts $ map (Gen.binaryOp "->") ts)
          vars = map (\i -> "x" <> show i) [1,2]
      expr <- do
        call <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "call")
        tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Tagged" (TidyM.importValue "tagged")
        tupleC <- Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor "Tuple0" "Tuple0")
        let idents = map Gen.exprIdent vars
            SolidityFunction{outputs} = f.solidityFunction
            callExpr = 
              Gen.exprApp call 
                [ unsafePartial $ unsafeIndex idents 0
                , unsafePartial $ unsafeIndex idents 1
                , Gen.exprParens $ Gen.exprTyped (Gen.exprParens $ Gen.exprApp tagged [tupleC]) (Gen.typeCtor f.typeName)
                ]
        if length outputs == 1
          then do
            untuple <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importValue "unTuple1")
            pure $ 
              Gen.exprOp (Gen.exprApp (Gen.exprIdent "map") [untuple]) 
                [ Gen.binaryOp "<$>" callExpr
                ]
          else pure callExpr
      pure $ [sig, Gen.declValue f.name (map Gen.binderVar vars) expr]

    mkArgsCall txOpts = unsafePartial do
      chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
      let sig = let ts = chainCursor : Gen.typeRecord f.factorTypes Nothing : [f.returnType]
                in Gen.declSignature f.name (Gen.typeOp txOpts $ map (Gen.binaryOp "->") ts)
          vars = map (\i -> "x" <> show i) [1,2,3]
      expr <- do
        uncurryFields <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3.Contract.Internal" (TidyM.importValue "uncurryFields")
        let helperName = f.name <> "'"
        pure $ 
          Gen.exprOp (Gen.exprApp uncurryFields [Gen.exprIdent $ unsafePartial $ unsafeIndex vars 2])
            [ Gen.binaryOp "$" 
              ( Gen.exprApp (Gen.exprIdent helperName) 
                  [ Gen.exprIdent $ unsafePartial $ unsafeIndex vars 0
                  , Gen.exprIdent $ unsafePartial $ unsafeIndex vars 1
                  ]
              )
            ]
      pure $ [sig, Gen.declValue f.name (map Gen.binderVar vars) expr]
      
  

mkHelperFunction
  :: forall m.
     Monad m
  => CodeOptions
  -> FunData
  -> { firstFactor :: Tuple String (CST.Type Void)
     , restFactors :: Array (Tuple String (CST.Type Void))
     }
  -> TidyM.CodegenT Void m (Array (CST.Declaration Void))
mkHelperFunction {exprPrefix} (FunData f) {firstFactor, restFactors} = unsafePartial do
  let SolidityFunction{constant, payable} = f.solidityFunction
  _txOpts <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "TransactionOptions")
  payAmt <- Gen.typeCtor <$> 
    if payable 
      then TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "NoPay")
      else TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "MinorUnit")
  let txOpts = Gen.typeApp _txOpts [payAmt]
      helperName = f.name <> "'"
  if constant 
     then mkCall helperName txOpts
     else mkSend helperName txOpts
  where
    -- NOTE: there is a lot of c/p between mkSend and mkCall but they ways in which they are different is just
    -- flat out annoying
    mkSend helperName txOpts = unsafePartial do
      firstFactor' <- tagInput firstFactor 
      restFactors' <- for restFactors tagInput
      let sig = let ts = (firstFactor' : restFactors') `snoc` f.returnType 
                in Gen.declSignature helperName (Gen.typeOp txOpts $ map (Gen.binaryOp "->") ts)
          -- the 3 comes from TxOpts -> firstFactor -> restFactors ->  returnType
          vars = map (\i -> "x" <> show i) (1 .. (length f.factorTypes + 2))
      expr <- do
        sendTx <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "sendTx")
        tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Tagged" (TidyM.importValue "tagged")
        tupleC <- do
          let SolidityFunction{outputs} = f.solidityFunction
              tupleType = "Tuple" <> show (length outputs)
          Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor tupleType tupleType)
        let idents = map Gen.exprIdent vars
        pure $ Gen.exprApp sendTx
          [ unsafePartial $ unsafeIndex idents 0
          , Gen.exprParens $ 
              Gen.exprTyped
                ( Gen.exprParens $ Gen.exprOp tagged 
                    [ Gen.binaryOp "$" $ Gen.exprApp tupleC (Array.drop 1 idents)
                    ]
                ) (Gen.typeCtor f.typeName)
          ]
      pure $ [sig, Gen.declValue helperName (map Gen.binderVar vars) expr]

    mkCall helperName txOpts = unsafePartial do
      chainCursor <- Gen.typeCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Types" (TidyM.importType "ChainCursor")
      firstFactor' <- tagInput firstFactor 
      restFactors' <- for restFactors tagInput
      let sig = let ts = (chainCursor : firstFactor' : restFactors') `snoc` f.returnType 
                in Gen.declSignature helperName (Gen.typeOp txOpts $ map (Gen.binaryOp "->") ts)
          -- the 3 comes from TxOpts -> ChainCursor -> firstFactor -> restFactors ->  returnType
          vars = map (\i -> "x" <> show i) (1 .. (length f.factorTypes + 3))
      expr <- do
        call <- Gen.exprIdent <$> TidyM.importFrom "Network.Ethereum.Web3" (TidyM.importValue "call")
        tagged <- Gen.exprIdent <$> TidyM.importFrom "Data.Tagged" (TidyM.importValue "tagged")
        tupleC <- do
          let SolidityFunction{outputs} = f.solidityFunction
              tupleType = "Tuple" <> show (length outputs)
          Gen.exprCtor <$> TidyM.importFrom "Network.Ethereum.Web3.Solidity" (TidyM.importCtor tupleType tupleType)
        let idents = map Gen.exprIdent vars
        pure $ Gen.exprApp call 
          [ unsafePartial $ unsafeIndex idents 0
          , unsafePartial $ unsafeIndex idents 1
          , Gen.exprParens $ 
              Gen.exprTyped
                ( Gen.exprParens $ Gen.exprOp tagged 
                    [ Gen.binaryOp "$" $ Gen.exprApp tupleC (Array.drop 2 idents)
                    ]
                ) (Gen.typeCtor f.typeName)
          ]
      pure $ [sig, Gen.declValue helperName (map Gen.binderVar vars) expr]

        

--------------------------------------------------------------------------------

data EventData =
  EventData { constructor :: String
            , indexedTypes :: Array (Tuple String (CST.Type Void))
            , nonIndexedTypes :: Array (Tuple String (CST.Type Void))
            , recordType :: Array (Tuple String (CST.Type Void))
            , solidityEvent :: SolidityEvent
            }

mkEventData
  :: forall m. 
     Monad m
  => SolidityEvent
  -> TidyM.CodegenT Void m EventData
mkEventData e@(SolidityEvent ev) = do
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
  pure $ EventData
    { constructor: if isValidType (capitalize ev.name) then capitalize ev.name else "EvT" <> ev.name
    , indexedTypes
    , nonIndexedTypes
    , recordType
    , solidityEvent: e
    }

eventDecls 
  :: forall m. 
     Monad m
  => EventData
  -> TidyM.CodegenT Void m (Array (CST.Declaration Void))
eventDecls (EventData decl) = unsafePartial do
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

  indexedEventDecl <- do 
    let f (Tuple name ty) = do
          tagged <- Gen.typeCtor <$> TidyM.importFrom "Data.Functor.Tagged" (TidyM.importType "Tagged")
          proxy <- Gen.typeCtor <$> TidyM.importFrom "Type.Proxy" (TidyM.importType "Proxy")
          pure $ Gen.typeApp tagged 
            [ Gen.typeApp proxy [Gen.typeString name]
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
    let SolidityEvent{anonymous} = decl.solidityEvent
    pure $
      Gen.declInstance Nothing [] indexedEventClass 
        [ Gen.typeApp indexedTuple $ map Gen.typeParens indexedTypesTagged
        , Gen.typeApp nonIndexedTuple $ map Gen.typeParens nonIndexedTypesTagged
        , Gen.typeCtor decl.constructor
        ]
        [ Gen.instValue "isAnonymous" [Gen.binderWildcard] (Gen.exprBool anonymous)
        ]

  eventFilterDecl <- do
    TidyM.importOpen "Prelude"
    {_fromJust, _just, _nothing} <-
      TidyM.importFrom "Data.Maybe"
        { _fromJust: TidyM.importValue "fromJust"
        , _just: TidyM.importCtor "Maybe" "Just"
        , _nothing: TidyM.importCtor "Maybe" "Nothing"
        }
    _unsafePartial <- TidyM.importFrom "Partial.Unsafe" (TidyM.importValue "unsafePartial")
    set <- TidyM.importFrom "Data.Lens" (TidyM.importValue "set")
    {_defaultFilter, _mkHexString} <- 
      TidyM.importFrom "Network.Ethereum.Web3.Types"
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
                                  , Gen.binaryOp "$" (Gen.exprApp (Gen.exprIdent _mkHexString) 
                                      [Gen.exprString $ unHex $ eventId decl.solidityEvent])
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
    , indexedEventDecl
    ]
  

--------------------------------------------------------------------------------

instance Monad m => Code (Abi Identity) m where
  genCode (Abi abi) opts = do
    codes <- for abi $ un Identity >>> case _ of
      -- AbiFunction f -> pure []
        -- functionCodeBlock <- funToFunctionCodeBlock f opts
        -- genCode functionCodeBlock opts
      AbiEvent e -> mkEventData e >>= eventDecls
      -- AbiConstructor (SolidityConstructor c) -> do
        -- let f = SolidityFunction { name : "constructor"
        --                          , inputs : c.inputs
        --                          , outputs : []
        --                          , constant : false
        --                          , payable : false
        --                          , isConstructor : true
        --                          , isUnCurried: c.isUnCurried
        --                          }
        -- functionCodeBlock <- funToFunctionCodeBlock f opts
        -- genCode functionCodeBlock opts
      _ -> pure []
      --AbiFallback _ ->
      --  -- Fallback is a function that gets called in case someone
      --  -- sends ether to the contract with no function specified
      --  -- so it's like, you would never call it on purpose, so we ignore it.
      --  pure ""
    pure $ concat codes

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
  | EventCodeBlock EventData  EventFilterInstance EventDecodeInstance EventGenericInstance

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
  genCode (EventCodeBlock decl@(EventData d) filterInst eventInst genericInst) opts = do
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

typeIsMultipart :: SolidityType -> Boolean
typeIsMultipart t = case t of
    SolidityBool -> false
    SolidityString -> false
    SolidityAddress -> false
    SolidityBytesD -> false
    _ -> true