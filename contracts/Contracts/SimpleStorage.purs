module Contracts.SimpleStorage where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (class EventFilter, _address, _topics, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, UIntN, Tuple0(..), Tuple1(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

newtype CountSet = CountSet { newCount :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype CountSet _
derive instance Generic CountSet _
instance Show CountSet where
  show = genericShow

instance Eq CountSet where
  eq = genericEq

instance EventFilter CountSet where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0 (Tuple1 (Tagged (Proxy "newCount") (UIntN (D2 :& D5 :& DOne D6)))) CountSet where
  isAnonymous _ = false

type CountFn = Tagged (Proxy "count()") Tuple0

count
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
count x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: CountFn)

type SetCountFn = Tagged (Proxy "setCount(uint256)")
  (Tuple1 (Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))))

setCount
  :: TransactionOptions MinorUnit -> { _count :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
setCount x1 x2 = uncurryFields x2 $ setCount' x1
  where
  setCount'
    :: TransactionOptions MinorUnit
    -> Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  setCount' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetCountFn)
