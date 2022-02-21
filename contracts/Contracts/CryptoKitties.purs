module Contracts.CryptoKitties where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple3, Tuple4, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple5(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void)
  (Tuple2 (Tagged (Proxy "_nftAddr") Address) (Tagged (Proxy "_cut") (UIntN (D2 :& D5 :& DOne D6))))

constructor
  :: TransactionOptions NoPay
  -> HexString
  -> { _nftAddr :: Address, _cut :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
constructor x1 x2 x3 = uncurryFields x3 $ constructor' x1 x2
  where
  constructor'
    :: TransactionOptions NoPay
    -> HexString
    -> Tagged (Proxy "_nftAddr") Address
    -> Tagged (Proxy "_cut") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  constructor' _x1 _x2 _x3 _x4 = deployContract _x1 _x2 (tagged $ Tuple2 _x3 _x4 :: ConstructorFn)

newtype AuctionCreated = AuctionCreated
  { tokenId :: UIntN (D2 :& D5 :& DOne D6)
  , startingPrice :: UIntN (D2 :& D5 :& DOne D6)
  , endingPrice :: UIntN (D2 :& D5 :& DOne D6)
  , duration :: UIntN (D2 :& D5 :& DOne D6)
  }

derive instance Newtype AuctionCreated _
derive instance Generic AuctionCreated _
instance Show AuctionCreated where
  show = genericShow

instance Eq AuctionCreated where
  eq = genericEq

instance EventFilter AuctionCreated where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "a9c8dfcda5664a5a124c713e386da27de87432d5b668e79458501eb296389ba7"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple4 (Tagged (Proxy "tokenId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "startingPrice") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "endingPrice") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "duration") (UIntN (D2 :& D5 :& DOne D6)))
    )
    AuctionCreated where
  isAnonymous _ = false

newtype AuctionSuccessful = AuctionSuccessful
  { tokenId :: UIntN (D2 :& D5 :& DOne D6)
  , totalPrice :: UIntN (D2 :& D5 :& DOne D6)
  , winner :: Address
  }

derive instance Newtype AuctionSuccessful _
derive instance Generic AuctionSuccessful _
instance Show AuctionSuccessful where
  show = genericShow

instance Eq AuctionSuccessful where
  eq = genericEq

instance EventFilter AuctionSuccessful where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "4fcc30d90a842164dd58501ab874a101a3749c3d4747139cefe7c876f4ccebd2"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple3 (Tagged (Proxy "tokenId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "totalPrice") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "winner") Address)
    )
    AuctionSuccessful where
  isAnonymous _ = false

newtype AuctionCancelled = AuctionCancelled { tokenId :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype AuctionCancelled _
derive instance Generic AuctionCancelled _
instance Show AuctionCancelled where
  show = genericShow

instance Eq AuctionCancelled where
  eq = genericEq

instance EventFilter AuctionCancelled where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "2809c7e17bf978fbc7194c0a694b638c4215e9140cacc6c38ca36010b45697df"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    (Tuple1 (Tagged (Proxy "tokenId") (UIntN (D2 :& D5 :& DOne D6))))
    AuctionCancelled where
  isAnonymous _ = false

newtype Pause = Pause {}

derive instance Newtype Pause _
derive instance Generic Pause _
instance Show Pause where
  show = genericShow

instance Eq Pause where
  eq = genericEq

instance EventFilter Pause where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "6985a02210a168e66602d3235cb6db0e70f92b3ba4d376a33c0f3d9434bff625"
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 Tuple0 Pause where
  isAnonymous _ = false

newtype Unpause = Unpause {}

derive instance Newtype Unpause _
derive instance Generic Unpause _
instance Show Unpause where
  show = genericShow

instance Eq Unpause where
  eq = genericEq

instance EventFilter Unpause where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "7805862f689e2f13df9f062ff482ad3ad112aca9e0847911ed832e158c525b33"
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 Tuple0 Unpause where
  isAnonymous _ = false

type BidFn = Tagged (Proxy "bid(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

bid
  :: TransactionOptions MinorUnit -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
bid x1 x2 = uncurryFields x2 $ bid' x1
  where
  bid'
    :: TransactionOptions MinorUnit
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  bid' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: BidFn)

type CancelAuctionFn = Tagged (Proxy "cancelAuction(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

cancelAuction
  :: TransactionOptions NoPay -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
cancelAuction x1 x2 = uncurryFields x2 $ cancelAuction' x1
  where
  cancelAuction'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  cancelAuction' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: CancelAuctionFn)

type CancelAuctionWhenPausedFn = Tagged (Proxy "cancelAuctionWhenPaused(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

cancelAuctionWhenPaused
  :: TransactionOptions NoPay -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
cancelAuctionWhenPaused x1 x2 = uncurryFields x2 $ cancelAuctionWhenPaused' x1
  where
  cancelAuctionWhenPaused'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  cancelAuctionWhenPaused' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: CancelAuctionWhenPausedFn)

type CreateAuctionFn = Tagged (Proxy "createAuction(uint256,uint256,uint256,uint256,address)")
  ( Tuple5 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_startingPrice") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_endingPrice") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_duration") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_seller") Address)
  )

createAuction
  :: TransactionOptions NoPay
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6)
     , _startingPrice :: UIntN (D2 :& D5 :& DOne D6)
     , _endingPrice :: UIntN (D2 :& D5 :& DOne D6)
     , _duration :: UIntN (D2 :& D5 :& DOne D6)
     , _seller :: Address
     }
  -> Web3 HexString
createAuction x1 x2 = uncurryFields x2 $ createAuction' x1
  where
  createAuction'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_startingPrice") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_endingPrice") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_duration") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_seller") Address
    -> Web3 HexString
  createAuction' _x1 _x2 _x3 _x4 _x5 _x6 = sendTx _x1
    (tagged $ Tuple5 _x2 _x3 _x4 _x5 _x6 :: CreateAuctionFn)

type GetAuctionFn = Tagged (Proxy "getAuction(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getAuction
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3
       ( Either CallError
           ( Tuple5 Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
           )
       )
getAuction x1 x2 x3 = uncurryFields x3 $ getAuction' x1 x2
  where
  getAuction'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3
         ( Either CallError
             ( Tuple5 Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
             )
         )
  getAuction' _x1 _x2 _x3 = call _x1 _x2 (tagged $ Tuple1 _x3 :: GetAuctionFn)

type GetCurrentPriceFn = Tagged (Proxy "getCurrentPrice(uint256)")
  (Tuple1 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getCurrentPrice
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getCurrentPrice x1 x2 x3 = uncurryFields x3 $ getCurrentPrice' x1 x2
  where
  getCurrentPrice'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  getCurrentPrice' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: GetCurrentPriceFn)

type IsSiringClockAuctionFn = Tagged (Proxy "isSiringClockAuction()") Tuple0

isSiringClockAuction :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
isSiringClockAuction x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: IsSiringClockAuctionFn)

type NonFungibleContractFn = Tagged (Proxy "nonFungibleContract()") Tuple0

nonFungibleContract :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
nonFungibleContract x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: NonFungibleContractFn)

type OwnerFn = Tagged (Proxy "owner()") Tuple0

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: OwnerFn)

type OwnerCutFn = Tagged (Proxy "ownerCut()") Tuple0

ownerCut
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
ownerCut x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: OwnerCutFn)

type PauseFn = Tagged (Proxy "pause()") Tuple0

pause :: TransactionOptions NoPay -> Web3 HexString
pause x1 = sendTx x1 (tagged Tuple0 :: PauseFn)

type PausedFn = Tagged (Proxy "paused()") Tuple0

paused :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
paused x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: PausedFn)

type TransferOwnershipFn = Tagged (Proxy "transferOwnership(address)")
  (Tuple1 (Tagged (Proxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x1 x2 = uncurryFields x2 $ transferOwnership' x1
  where
  transferOwnership'
    :: TransactionOptions NoPay -> Tagged (Proxy "newOwner") Address -> Web3 HexString
  transferOwnership' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: TransferOwnershipFn)

type UnpauseFn = Tagged (Proxy "unpause()") Tuple0

unpause :: TransactionOptions NoPay -> Web3 HexString
unpause x1 = sendTx x1 (tagged Tuple0 :: UnpauseFn)

type WithdrawBalanceFn = Tagged (Proxy "withdrawBalance()") Tuple0

withdrawBalance :: TransactionOptions NoPay -> Web3 HexString
withdrawBalance x1 = sendTx x1 (tagged Tuple0 :: WithdrawBalanceFn)
