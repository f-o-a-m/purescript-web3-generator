module ContractsTruffle.ERC721 where

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
import Network.Ethereum.Web3.Solidity (ByteString, D2, D5, D6, DOne, UIntN, Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void)
  (Tuple1 (Tagged (Proxy "totalSupply") (UIntN (D2 :& D5 :& DOne D6))))

constructor
  :: TransactionOptions NoPay
  -> HexString
  -> { totalSupply :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
constructor x1 x2 x3 = uncurryFields x3 $ constructor' x1 x2
  where
  constructor'
    :: TransactionOptions NoPay
    -> HexString
    -> Tagged (Proxy "totalSupply") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  constructor' _x1 _x2 _x3 = deployContract _x1 _x2 (tagged $ Tuple1 _x3 :: ConstructorFn)

newtype Transfer = Transfer
  { _from :: Address, _to :: Address, _tokenId :: UIntN (D2 :& D5 :& DOne D6) }

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
    (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))
    Transfer where
  isAnonymous _ = false

newtype Approval = Approval
  { _owner :: Address, _approved :: Address, _tokenId :: UIntN (D2 :& D5 :& DOne D6) }

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
  IndexedEvent (Tuple2 (Tagged (Proxy "_owner") Address) (Tagged (Proxy "_approved") Address))
    (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))
    Approval where
  isAnonymous _ = false

newtype ApprovalForAll = ApprovalForAll
  { _owner :: Address, _operator :: Address, _approved :: Boolean }

derive instance Newtype ApprovalForAll _
derive instance Generic ApprovalForAll _
instance Show ApprovalForAll where
  show = genericShow

instance Eq ApprovalForAll where
  eq = genericEq

instance EventFilter ApprovalForAll where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent (Tuple2 (Tagged (Proxy "_owner") Address) (Tagged (Proxy "_operator") Address))
    (Tuple1 (Tagged (Proxy "_approved") Boolean))
    ApprovalForAll where
  isAnonymous _ = false

type ApproveFn = Tagged (Proxy "approve(address,uint256)")
  (Tuple2 (Tagged (Proxy "_to") Address) (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

approve
  :: TransactionOptions NoPay
  -> { _to :: Address, _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
approve x1 x2 = uncurryFields x2 $ approve' x1
  where
  approve'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
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

type ExistsFn = Tagged (Proxy "exists(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

exists
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError Boolean)
exists x1 x2 x3 = uncurryFields x3 $ exists' x1 x2
  where
  exists'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError Boolean)
  exists' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: ExistsFn)

type GetApprovedFn = Tagged (Proxy "getApproved(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getApproved
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError Address)
getApproved x1 x2 x3 = uncurryFields x3 $ getApproved' x1 x2
  where
  getApproved'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError Address)
  getApproved' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: GetApprovedFn)

type IsApprovedForAllFn = Tagged (Proxy "isApprovedForAll(address,address)")
  (Tuple2 (Tagged (Proxy "_owner") Address) (Tagged (Proxy "_operator") Address))

isApprovedForAll
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _owner :: Address, _operator :: Address }
  -> Web3 (Either CallError Boolean)
isApprovedForAll x1 x2 x3 = uncurryFields x3 $ isApprovedForAll' x1 x2
  where
  isApprovedForAll'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_owner") Address
    -> Tagged (Proxy "_operator") Address
    -> Web3 (Either CallError Boolean)
  isApprovedForAll' _x1 _x2 _x3 _x4 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple2 _x3 _x4 :: IsApprovedForAllFn)

type OwnerOfFn = Tagged (Proxy "ownerOf(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

ownerOf
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError Address)
ownerOf x1 x2 x3 = uncurryFields x3 $ ownerOf' x1 x2
  where
  ownerOf'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError Address)
  ownerOf' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: OwnerOfFn)

type SafeTransferFrom4Fn = Tagged (Proxy "safeTransferFrom4(address,address,uint256,bytes)")
  ( Tuple4 (Tagged (Proxy "_from") Address) (Tagged (Proxy "_to") Address)
      (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_data") ByteString)
  )

safeTransferFrom4
  :: TransactionOptions NoPay
  -> { _from :: Address
     , _to :: Address
     , _tokenId :: UIntN (D2 :& D5 :& DOne D6)
     , _data :: ByteString
     }
  -> Web3 HexString
safeTransferFrom4 x1 x2 = uncurryFields x2 $ safeTransferFrom4' x1
  where
  safeTransferFrom4'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_from") Address
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_data") ByteString
    -> Web3 HexString
  safeTransferFrom4' _x1 _x2 _x3 _x4 _x5 = sendTx _x1
    (tagged $ Tuple4 _x2 _x3 _x4 _x5 :: SafeTransferFrom4Fn)

type SafeTransferFrom3Fn = Tagged (Proxy "safeTransferFrom3(address,address,uint256)")
  ( Tuple3 (Tagged (Proxy "_from") Address) (Tagged (Proxy "_to") Address)
      (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))
  )

safeTransferFrom3
  :: TransactionOptions NoPay
  -> { _from :: Address, _to :: Address, _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
safeTransferFrom3 x1 x2 = uncurryFields x2 $ safeTransferFrom3' x1
  where
  safeTransferFrom3'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_from") Address
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  safeTransferFrom3' _x1 _x2 _x3 _x4 = sendTx _x1
    (tagged $ Tuple3 _x2 _x3 _x4 :: SafeTransferFrom3Fn)

type SetApprovalForAllFn = Tagged (Proxy "setApprovalForAll(address,bool)")
  (Tuple2 (Tagged (Proxy "_to") Address) (Tagged (Proxy "_approved") Boolean))

setApprovalForAll
  :: TransactionOptions NoPay -> { _to :: Address, _approved :: Boolean } -> Web3 HexString
setApprovalForAll x1 x2 = uncurryFields x2 $ setApprovalForAll' x1
  where
  setApprovalForAll'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_approved") Boolean
    -> Web3 HexString
  setApprovalForAll' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: SetApprovalForAllFn)

type TransferFromFn = Tagged (Proxy "transferFrom(address,address,uint256)")
  ( Tuple3 (Tagged (Proxy "_from") Address) (Tagged (Proxy "_to") Address)
      (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))
  )

transferFrom
  :: TransactionOptions NoPay
  -> { _from :: Address, _to :: Address, _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
transferFrom x1 x2 = uncurryFields x2 $ transferFrom' x1
  where
  transferFrom'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_from") Address
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  transferFrom' _x1 _x2 _x3 _x4 = sendTx _x1 (tagged $ Tuple3 _x2 _x3 _x4 :: TransferFromFn)
