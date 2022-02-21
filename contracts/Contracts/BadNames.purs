module Contracts.BadNames where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (class EventFilter, _address, _topics, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0, UIntN, Tuple1(..), class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

newtype EvT_BadEventName = EvT_BadEventName { n :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype EvT_BadEventName _
derive instance Generic EvT_BadEventName _
instance Show EvT_BadEventName where
  show = genericShow

instance Eq EvT_BadEventName where
  eq = genericEq

instance EventFilter EvT_BadEventName where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "0a74e27e86665d15520ffa6f5318114704bde7889bbee91711c5153b43401abd"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0 (Tuple1 (Tagged (Proxy "n") (UIntN (D2 :& D5 :& DOne D6)))) EvT_BadEventName where
  isAnonymous _ = false

type FnT_dumbFunctionFn = Tagged (Proxy "_dumbFunction(uint256)")
  (Tuple1 (Tagged (Proxy "n") (UIntN (D2 :& D5 :& DOne D6))))

_dumbFunction :: TransactionOptions NoPay -> { n :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
_dumbFunction x1 x2 = uncurryFields x2 $ _dumbFunction' x1
  where
  _dumbFunction'
    :: TransactionOptions NoPay -> Tagged (Proxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 HexString
  _dumbFunction' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: FnT_dumbFunctionFn)
