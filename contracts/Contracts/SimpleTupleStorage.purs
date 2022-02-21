module Contracts.SimpleTupleStorage where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (class EventFilter, _address, _topics, call, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, UIntN, Tuple0(..), Tuple2(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void) Tuple0

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x1 x2 = deployContract x1 x2 (tagged Tuple0 :: ConstructorFn)

newtype Stored = Stored { x :: UIntN (D2 :& D5 :& DOne D6), y :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype Stored _
derive instance Generic Stored _
instance Show Stored where
  show = genericShow

instance Eq Stored where
  eq = genericEq

instance EventFilter Stored where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "793333530bc27d845c36d8799afd0c1152c33d074b4419780fc2a82658267614"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple2 (Tagged (Proxy "x") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "y") (UIntN (D2 :& D5 :& DOne D6)))
    )
    Stored where
  isAnonymous _ = false

type GetTupleFn = Tagged (Proxy "getTuple()") Tuple0

getTuple
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
getTuple x1 x2 = call x1 x2 (tagged Tuple0 :: GetTupleFn)

type SetTupleFn = Tagged (Proxy "setTuple(uint256,uint256)")
  ( Tuple2 (Tagged (Proxy "x") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "y") (UIntN (D2 :& D5 :& DOne D6)))
  )

setTuple
  :: TransactionOptions NoPay
  -> { x :: UIntN (D2 :& D5 :& DOne D6), y :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
setTuple x1 x2 = uncurryFields x2 $ setTuple' x1
  where
  setTuple'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "x") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "y") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  setTuple' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: SetTupleFn)

type StoredDataXFn = Tagged (Proxy "storedDataX()") Tuple0

storedDataX
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
storedDataX x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: StoredDataXFn)

type StoredDataYFn = Tagged (Proxy "storedDataY()") Tuple0

storedDataY
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
storedDataY x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: StoredDataYFn)
