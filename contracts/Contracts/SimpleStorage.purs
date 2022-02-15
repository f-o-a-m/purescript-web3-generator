--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

module Contracts.SimpleStorage where

import Prelude 

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)
--------------------------------------------------------------------------------
-- | CountSet
--------------------------------------------------------------------------------


newtype CountSet = CountSet {newCount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeCountSet :: Newtype CountSet _

instance eventFilterCountSet :: EventFilter CountSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]

instance indexedEventCountSet :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (Proxy "newCount") (UIntN (D2 :& D5 :& DOne D6)))) CountSet where
  isAnonymous _ = false

derive instance genericCountSet :: Generic CountSet _

instance eventGenericCountSetShow :: Show CountSet where
  show = genericShow

instance eventGenericCountSeteq :: Eq CountSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | CountFn
--------------------------------------------------------------------------------


type CountFn = Tagged (Proxy "count()") (Tuple0 )

count :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
count x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CountFn)

--------------------------------------------------------------------------------
-- | SetCountFn
--------------------------------------------------------------------------------


type SetCountFn = Tagged (Proxy "setCount(uint256)") (Tuple1 (Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))))

setCount :: TransactionOptions MinorUnit -> { _count :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setCount x0 r = uncurryFields  r $ setCount' x0
   where
    setCount' :: TransactionOptions MinorUnit -> (Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setCount' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetCountFn)