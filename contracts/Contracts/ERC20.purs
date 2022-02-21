module Contracts.ERC20 where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void)
  ( Tuple4 (Tagged (Proxy "_name") String) (Tagged (Proxy "_symbol") String)
      (Tagged (Proxy "_decimals") (UIntN (DOne D8)))
      (Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6)))
  )

constructor
  :: TransactionOptions NoPay
  -> HexString
  -> { _name :: String
     , _symbol :: String
     , _decimals :: UIntN (DOne D8)
     , _count :: UIntN (D2 :& D5 :& DOne D6)
     }
  -> Web3 HexString
constructor x1 x2 x3 = uncurryFields x3 $ constructor' x1 x2
  where
  constructor'
    :: TransactionOptions NoPay
    -> HexString
    -> Tagged (Proxy "_name") String
    -> Tagged (Proxy "_symbol") String
    -> Tagged (Proxy "_decimals") (UIntN (DOne D8))
    -> Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  constructor' _x1 _x2 _x3 _x4 _x5 _x6 = deployContract _x1 _x2
    (tagged $ Tuple4 _x3 _x4 _x5 _x6 :: ConstructorFn)

newtype Transfer = Transfer
  { _from :: Address, _to :: Address, _value :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype Transfer _
derive instance Generic Transfer _
instance Show Transfer where
  show = genericShow

instance Eq Transfer where
  eq = genericEq

instance EventFilter Transfer where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent (Tuple2 (Tagged (Proxy "_from") Address) (Tagged (Proxy "_to") Address))
    (Tuple1 (Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6))))
    Transfer where
  isAnonymous _ = false

newtype Approval = Approval
  { _owner :: Address, _spender :: Address, _value :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype Approval _
derive instance Generic Approval _
instance Show Approval where
  show = genericShow

instance Eq Approval where
  eq = genericEq

instance EventFilter Approval where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent (Tuple2 (Tagged (Proxy "_owner") Address) (Tagged (Proxy "_spender") Address))
    (Tuple1 (Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6))))
    Approval where
  isAnonymous _ = false

type AllowanceFn = Tagged (Proxy "allowance(address,address)")
  (Tuple2 (Tagged (Proxy "_owner") Address) (Tagged (Proxy "_spender") Address))

allowance
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _owner :: Address, _spender :: Address }
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
allowance x1 x2 x3 = uncurryFields x3 $ allowance' x1 x2
  where
  allowance'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_owner") Address
    -> Tagged (Proxy "_spender") Address
    -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  allowance' _x1 _x2 _x3 _x4 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple2 _x3 _x4 :: AllowanceFn)

type ApproveFn = Tagged (Proxy "approve(address,uint256)")
  ( Tuple2 (Tagged (Proxy "_spender") Address)
      (Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6)))
  )

approve
  :: TransactionOptions NoPay
  -> { _spender :: Address, _value :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
approve x1 x2 = uncurryFields x2 $ approve' x1
  where
  approve'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_spender") Address
    -> Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  approve' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: ApproveFn)

type BalanceOfFn = Tagged (Proxy "balanceOf(address)") (Tuple1 (Tagged (Proxy "_owner") Address))

balanceOf
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _owner :: Address }
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x1 x2 x3 = uncurryFields x3 $ balanceOf' x1 x2
  where
  balanceOf'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_owner") Address
    -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  balanceOf' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: BalanceOfFn)

type DecimalsFn = Tagged (Proxy "decimals()") Tuple0

decimals :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
decimals x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: DecimalsFn)

type DelegateFn = Tagged (Proxy "delegate(address)") (Tuple1 (Tagged (Proxy "_owner") Address))

delegate :: TransactionOptions NoPay -> { _owner :: Address } -> Web3 HexString
delegate x1 x2 = uncurryFields x2 $ delegate' x1
  where
  delegate' :: TransactionOptions NoPay -> Tagged (Proxy "_owner") Address -> Web3 HexString
  delegate' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: DelegateFn)

type KillFn = Tagged (Proxy "kill()") Tuple0

kill :: TransactionOptions NoPay -> Web3 HexString
kill x1 = sendTx x1 (tagged Tuple0 :: KillFn)

type NameFn = Tagged (Proxy "name()") Tuple0

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: NameFn)

type OwnerFn = Tagged (Proxy "owner()") Tuple0

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: OwnerFn)

type SymbolFn = Tagged (Proxy "symbol()") Tuple0

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: SymbolFn)

type TotalSupplyFn = Tagged (Proxy "totalSupply()") Tuple0

totalSupply
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalSupply x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: TotalSupplyFn)

type TransferFn = Tagged (Proxy "transfer(address,uint256)")
  (Tuple2 (Tagged (Proxy "_to") Address) (Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6))))

transfer
  :: TransactionOptions NoPay
  -> { _to :: Address, _value :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
transfer x1 x2 = uncurryFields x2 $ transfer' x1
  where
  transfer'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  transfer' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: TransferFn)

type TransferFromFn = Tagged (Proxy "transferFrom(address,address,uint256)")
  ( Tuple3 (Tagged (Proxy "_from") Address) (Tagged (Proxy "_to") Address)
      (Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6)))
  )

transferFrom
  :: TransactionOptions NoPay
  -> { _from :: Address, _to :: Address, _value :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
transferFrom x1 x2 = uncurryFields x2 $ transferFrom' x1
  where
  transferFrom'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_from") Address
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_value") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  transferFrom' _x1 _x2 _x3 _x4 = sendTx _x1 (tagged $ Tuple3 _x2 _x3 _x4 :: TransferFromFn)

type UnapproveFn = Tagged (Proxy "unapprove(address)") (Tuple1 (Tagged (Proxy "_spender") Address))

unapprove :: TransactionOptions NoPay -> { _spender :: Address } -> Web3 HexString
unapprove x1 x2 = uncurryFields x2 $ unapprove' x1
  where
  unapprove' :: TransactionOptions NoPay -> Tagged (Proxy "_spender") Address -> Web3 HexString
  unapprove' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: UnapproveFn)
