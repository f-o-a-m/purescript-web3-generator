module Contracts.FoamTokenFaucet where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0, Tuple2, Tuple3, UIntN, Tuple1(..), class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

newtype TokenChange = TokenChange { _old :: Address, _new :: Address }

derive instance Newtype TokenChange _
derive instance Generic TokenChange _
instance Show TokenChange where
  show = genericShow

instance Eq TokenChange where
  eq = genericEq

instance EventFilter TokenChange where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "2ef4615f87b614ed8e8f7a775a271837b9999df8280dfd816d28c37498ca35a4"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    (Tuple2 (Tagged (Proxy "_old") Address) (Tagged (Proxy "_new") Address))
    TokenChange where
  isAnonymous _ = false

newtype PreTransfer = PreTransfer
  { _from :: Address, _to :: Address, _value :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype PreTransfer _
derive instance Generic PreTransfer _
instance Show PreTransfer where
  show = genericShow

instance Eq PreTransfer where
  eq = genericEq

instance EventFilter PreTransfer where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "6db37187401e2ebf075951204867ea0cfa6150cfd705f503acf8d99105c79b64"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple3 (Tagged (Proxy "_from") Address) (Tagged (Proxy "_to") Address)
        (Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6)))
    )
    PreTransfer where
  isAnonymous _ = false

type FaucetFn = Tagged (Proxy "faucet(address)") (Tuple1 (Tagged (Proxy "to") Address))

faucet :: TransactionOptions MinorUnit -> { to :: Address } -> Web3 HexString
faucet x1 x2 = uncurryFields x2 $ faucet' x1
  where
  faucet' :: TransactionOptions MinorUnit -> Tagged (Proxy "to") Address -> Web3 HexString
  faucet' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: FaucetFn)
