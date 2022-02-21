module Contracts.KittyCore where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D5, D6, DOne, Tuple10, Tuple5, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void) Tuple0

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x1 x2 = deployContract x1 x2 (tagged Tuple0 :: ConstructorFn)

newtype Pregnant = Pregnant
  { owner :: Address
  , matronId :: UIntN (D2 :& D5 :& DOne D6)
  , sireId :: UIntN (D2 :& D5 :& DOne D6)
  , cooldownEndBlock :: UIntN (D2 :& D5 :& DOne D6)
  }

derive instance Newtype Pregnant _
derive instance Generic Pregnant _
instance Show Pregnant where
  show = genericShow

instance Eq Pregnant where
  eq = genericEq

instance EventFilter Pregnant where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "241ea03ca20251805084d27d4440371c34a0b85ff108f6bb5611248f73818b80"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple4 (Tagged (Proxy "owner") Address)
        (Tagged (Proxy "matronId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "sireId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "cooldownEndBlock") (UIntN (D2 :& D5 :& DOne D6)))
    )
    Pregnant where
  isAnonymous _ = false

newtype Transfer = Transfer
  { from :: Address, to :: Address, tokenId :: UIntN (D2 :& D5 :& DOne D6) }

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
  IndexedEvent Tuple0
    ( Tuple3 (Tagged (Proxy "from") Address) (Tagged (Proxy "to") Address)
        (Tagged (Proxy "tokenId") (UIntN (D2 :& D5 :& DOne D6)))
    )
    Transfer where
  isAnonymous _ = false

newtype Approval = Approval
  { owner :: Address, approved :: Address, tokenId :: UIntN (D2 :& D5 :& DOne D6) }

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
  IndexedEvent Tuple0
    ( Tuple3 (Tagged (Proxy "owner") Address) (Tagged (Proxy "approved") Address)
        (Tagged (Proxy "tokenId") (UIntN (D2 :& D5 :& DOne D6)))
    )
    Approval where
  isAnonymous _ = false

newtype Birth = Birth
  { owner :: Address
  , kittyId :: UIntN (D2 :& D5 :& DOne D6)
  , matronId :: UIntN (D2 :& D5 :& DOne D6)
  , sireId :: UIntN (D2 :& D5 :& DOne D6)
  , genes :: UIntN (D2 :& D5 :& DOne D6)
  }

derive instance Newtype Birth _
derive instance Generic Birth _
instance Show Birth where
  show = genericShow

instance Eq Birth where
  eq = genericEq

instance EventFilter Birth where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "0a5311bd2a6608f08a180df2ee7c5946819a649b204b554bb8e39825b2c50ad5"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple5 (Tagged (Proxy "owner") Address)
        (Tagged (Proxy "kittyId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "matronId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "sireId") (UIntN (D2 :& D5 :& DOne D6)))
        (Tagged (Proxy "genes") (UIntN (D2 :& D5 :& DOne D6)))
    )
    Birth where
  isAnonymous _ = false

newtype ContractUpgrade = ContractUpgrade { newContract :: Address }

derive instance Newtype ContractUpgrade _
derive instance Generic ContractUpgrade _
instance Show ContractUpgrade where
  show = genericShow

instance Eq ContractUpgrade where
  eq = genericEq

instance EventFilter ContractUpgrade where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "450db8da6efbe9c22f2347f7c2021231df1fc58d3ae9a2fa75d39fa446199305"
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 (Tuple1 (Tagged (Proxy "newContract") Address)) ContractUpgrade where
  isAnonymous _ = false

type GEN0_AUCTION_DURATIONFn = Tagged (Proxy "GEN0_AUCTION_DURATION()") Tuple0

gEN0_AUCTION_DURATION
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
gEN0_AUCTION_DURATION x1 x2 = map unTuple1 <$> call x1 x2
  (tagged Tuple0 :: GEN0_AUCTION_DURATIONFn)

type GEN0_CREATION_LIMITFn = Tagged (Proxy "GEN0_CREATION_LIMIT()") Tuple0

gEN0_CREATION_LIMIT
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
gEN0_CREATION_LIMIT x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: GEN0_CREATION_LIMITFn)

type GEN0_STARTING_PRICEFn = Tagged (Proxy "GEN0_STARTING_PRICE()") Tuple0

gEN0_STARTING_PRICE
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
gEN0_STARTING_PRICE x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: GEN0_STARTING_PRICEFn)

type PROMO_CREATION_LIMITFn = Tagged (Proxy "PROMO_CREATION_LIMIT()") Tuple0

pROMO_CREATION_LIMIT
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
pROMO_CREATION_LIMIT x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: PROMO_CREATION_LIMITFn)

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

type ApproveSiringFn = Tagged (Proxy "approveSiring(address,uint256)")
  (Tuple2 (Tagged (Proxy "_addr") Address) (Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6))))

approveSiring
  :: TransactionOptions NoPay
  -> { _addr :: Address, _sireId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
approveSiring x1 x2 = uncurryFields x2 $ approveSiring' x1
  where
  approveSiring'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_addr") Address
    -> Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  approveSiring' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: ApproveSiringFn)

type AutoBirthFeeFn = Tagged (Proxy "autoBirthFee()") Tuple0

autoBirthFee
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
autoBirthFee x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: AutoBirthFeeFn)

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

type BidOnSiringAuctionFn = Tagged (Proxy "bidOnSiringAuction(uint256,uint256)")
  ( Tuple2 (Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6)))
  )

bidOnSiringAuction
  :: TransactionOptions MinorUnit
  -> { _sireId :: UIntN (D2 :& D5 :& DOne D6), _matronId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
bidOnSiringAuction x1 x2 = uncurryFields x2 $ bidOnSiringAuction' x1
  where
  bidOnSiringAuction'
    :: TransactionOptions MinorUnit
    -> Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  bidOnSiringAuction' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: BidOnSiringAuctionFn)

type BreedWithAutoFn = Tagged (Proxy "breedWithAuto(uint256,uint256)")
  ( Tuple2 (Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6)))
  )

breedWithAuto
  :: TransactionOptions MinorUnit
  -> { _matronId :: UIntN (D2 :& D5 :& DOne D6), _sireId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
breedWithAuto x1 x2 = uncurryFields x2 $ breedWithAuto' x1
  where
  breedWithAuto'
    :: TransactionOptions MinorUnit
    -> Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  breedWithAuto' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: BreedWithAutoFn)

type CanBreedWithFn = Tagged (Proxy "canBreedWith(uint256,uint256)")
  ( Tuple2 (Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6)))
  )

canBreedWith
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _matronId :: UIntN (D2 :& D5 :& DOne D6), _sireId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError Boolean)
canBreedWith x1 x2 x3 = uncurryFields x3 $ canBreedWith' x1 x2
  where
  canBreedWith'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_sireId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError Boolean)
  canBreedWith' _x1 _x2 _x3 _x4 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple2 _x3 _x4 :: CanBreedWithFn)

type CeoAddressFn = Tagged (Proxy "ceoAddress()") Tuple0

ceoAddress :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
ceoAddress x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: CeoAddressFn)

type CfoAddressFn = Tagged (Proxy "cfoAddress()") Tuple0

cfoAddress :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
cfoAddress x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: CfoAddressFn)

type CooAddressFn = Tagged (Proxy "cooAddress()") Tuple0

cooAddress :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
cooAddress x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: CooAddressFn)

type CooldownsFn = Tagged (Proxy "cooldowns(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

cooldowns
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError (UIntN (D3 :& DOne D2)))
cooldowns x1 x2 x3 = cooldowns' x1 x2 x3
  where
  cooldowns'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError (UIntN (D3 :& DOne D2)))
  cooldowns' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: CooldownsFn)

type CreateGen0AuctionFn = Tagged (Proxy "createGen0Auction(uint256)")
  (Tuple1 (Tagged (Proxy "_genes") (UIntN (D2 :& D5 :& DOne D6))))

createGen0Auction
  :: TransactionOptions NoPay -> { _genes :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
createGen0Auction x1 x2 = uncurryFields x2 $ createGen0Auction' x1
  where
  createGen0Auction'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_genes") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  createGen0Auction' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: CreateGen0AuctionFn)

type CreatePromoKittyFn = Tagged (Proxy "createPromoKitty(uint256,address)")
  (Tuple2 (Tagged (Proxy "_genes") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (Proxy "_owner") Address))

createPromoKitty
  :: TransactionOptions NoPay
  -> { _genes :: UIntN (D2 :& D5 :& DOne D6), _owner :: Address }
  -> Web3 HexString
createPromoKitty x1 x2 = uncurryFields x2 $ createPromoKitty' x1
  where
  createPromoKitty'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_genes") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_owner") Address
    -> Web3 HexString
  createPromoKitty' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: CreatePromoKittyFn)

type CreateSaleAuctionFn = Tagged (Proxy "createSaleAuction(uint256,uint256,uint256,uint256)")
  ( Tuple4 (Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_startingPrice") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_endingPrice") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_duration") (UIntN (D2 :& D5 :& DOne D6)))
  )

createSaleAuction
  :: TransactionOptions NoPay
  -> { _kittyId :: UIntN (D2 :& D5 :& DOne D6)
     , _startingPrice :: UIntN (D2 :& D5 :& DOne D6)
     , _endingPrice :: UIntN (D2 :& D5 :& DOne D6)
     , _duration :: UIntN (D2 :& D5 :& DOne D6)
     }
  -> Web3 HexString
createSaleAuction x1 x2 = uncurryFields x2 $ createSaleAuction' x1
  where
  createSaleAuction'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_startingPrice") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_endingPrice") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_duration") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  createSaleAuction' _x1 _x2 _x3 _x4 _x5 = sendTx _x1
    (tagged $ Tuple4 _x2 _x3 _x4 _x5 :: CreateSaleAuctionFn)

type CreateSiringAuctionFn = Tagged (Proxy "createSiringAuction(uint256,uint256,uint256,uint256)")
  ( Tuple4 (Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_startingPrice") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_endingPrice") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_duration") (UIntN (D2 :& D5 :& DOne D6)))
  )

createSiringAuction
  :: TransactionOptions NoPay
  -> { _kittyId :: UIntN (D2 :& D5 :& DOne D6)
     , _startingPrice :: UIntN (D2 :& D5 :& DOne D6)
     , _endingPrice :: UIntN (D2 :& D5 :& DOne D6)
     , _duration :: UIntN (D2 :& D5 :& DOne D6)
     }
  -> Web3 HexString
createSiringAuction x1 x2 = uncurryFields x2 $ createSiringAuction' x1
  where
  createSiringAuction'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_startingPrice") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_endingPrice") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_duration") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  createSiringAuction' _x1 _x2 _x3 _x4 _x5 = sendTx _x1
    (tagged $ Tuple4 _x2 _x3 _x4 _x5 :: CreateSiringAuctionFn)

type Erc721MetadataFn = Tagged (Proxy "erc721Metadata()") Tuple0

erc721Metadata :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
erc721Metadata x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: Erc721MetadataFn)

type Gen0CreatedCountFn = Tagged (Proxy "gen0CreatedCount()") Tuple0

gen0CreatedCount
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
gen0CreatedCount x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: Gen0CreatedCountFn)

type GeneScienceFn = Tagged (Proxy "geneScience()") Tuple0

geneScience :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
geneScience x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: GeneScienceFn)

type GetKittyFn = Tagged (Proxy "getKitty(uint256)")
  (Tuple1 (Tagged (Proxy "_id") (UIntN (D2 :& D5 :& DOne D6))))

getKitty
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _id :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3
       ( Either CallError
           ( Tuple10 Boolean Boolean (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
               (UIntN (D2 :& D5 :& DOne D6))
           )
       )
getKitty x1 x2 x3 = uncurryFields x3 $ getKitty' x1 x2
  where
  getKitty'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_id") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3
         ( Either CallError
             ( Tuple10 Boolean Boolean (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
                 (UIntN (D2 :& D5 :& DOne D6))
             )
         )
  getKitty' _x1 _x2 _x3 = call _x1 _x2 (tagged $ Tuple1 _x3 :: GetKittyFn)

type GiveBirthFn = Tagged (Proxy "giveBirth(uint256)")
  (Tuple1 (Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6))))

giveBirth
  :: TransactionOptions NoPay -> { _matronId :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
giveBirth x1 x2 = uncurryFields x2 $ giveBirth' x1
  where
  giveBirth'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_matronId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  giveBirth' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: GiveBirthFn)

type IsPregnantFn = Tagged (Proxy "isPregnant(uint256)")
  (Tuple1 (Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6))))

isPregnant
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _kittyId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError Boolean)
isPregnant x1 x2 x3 = uncurryFields x3 $ isPregnant' x1 x2
  where
  isPregnant'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError Boolean)
  isPregnant' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: IsPregnantFn)

type IsReadyToBreedFn = Tagged (Proxy "isReadyToBreed(uint256)")
  (Tuple1 (Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6))))

isReadyToBreed
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _kittyId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 (Either CallError Boolean)
isReadyToBreed x1 x2 x3 = uncurryFields x3 $ isReadyToBreed' x1 x2
  where
  isReadyToBreed'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_kittyId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 (Either CallError Boolean)
  isReadyToBreed' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: IsReadyToBreedFn)

type KittyIndexToApprovedFn = Tagged (Proxy "kittyIndexToApproved(uint256)")
  (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

kittyIndexToApproved
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError Address)
kittyIndexToApproved x1 x2 x3 = kittyIndexToApproved' x1 x2 x3
  where
  kittyIndexToApproved'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError Address)
  kittyIndexToApproved' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: KittyIndexToApprovedFn)

type KittyIndexToOwnerFn = Tagged (Proxy "kittyIndexToOwner(uint256)")
  (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

kittyIndexToOwner
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError Address)
kittyIndexToOwner x1 x2 x3 = kittyIndexToOwner' x1 x2 x3
  where
  kittyIndexToOwner'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError Address)
  kittyIndexToOwner' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: KittyIndexToOwnerFn)

type NameFn = Tagged (Proxy "name()") Tuple0

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: NameFn)

type NewContractAddressFn = Tagged (Proxy "newContractAddress()") Tuple0

newContractAddress :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
newContractAddress x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: NewContractAddressFn)

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

type PauseFn = Tagged (Proxy "pause()") Tuple0

pause :: TransactionOptions NoPay -> Web3 HexString
pause x1 = sendTx x1 (tagged Tuple0 :: PauseFn)

type PausedFn = Tagged (Proxy "paused()") Tuple0

paused :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
paused x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: PausedFn)

type PregnantKittiesFn = Tagged (Proxy "pregnantKitties()") Tuple0

pregnantKitties
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
pregnantKitties x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: PregnantKittiesFn)

type PromoCreatedCountFn = Tagged (Proxy "promoCreatedCount()") Tuple0

promoCreatedCount
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
promoCreatedCount x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: PromoCreatedCountFn)

type SaleAuctionFn = Tagged (Proxy "saleAuction()") Tuple0

saleAuction :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
saleAuction x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: SaleAuctionFn)

type SecondsPerBlockFn = Tagged (Proxy "secondsPerBlock()") Tuple0

secondsPerBlock
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
secondsPerBlock x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: SecondsPerBlockFn)

type SetAutoBirthFeeFn = Tagged (Proxy "setAutoBirthFee(uint256)")
  (Tuple1 (Tagged (Proxy "val") (UIntN (D2 :& D5 :& DOne D6))))

setAutoBirthFee
  :: TransactionOptions NoPay -> { val :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
setAutoBirthFee x1 x2 = uncurryFields x2 $ setAutoBirthFee' x1
  where
  setAutoBirthFee'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "val") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  setAutoBirthFee' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetAutoBirthFeeFn)

type SetCEOFn = Tagged (Proxy "setCEO(address)") (Tuple1 (Tagged (Proxy "_newCEO") Address))

setCEO :: TransactionOptions NoPay -> { _newCEO :: Address } -> Web3 HexString
setCEO x1 x2 = uncurryFields x2 $ setCEO' x1
  where
  setCEO' :: TransactionOptions NoPay -> Tagged (Proxy "_newCEO") Address -> Web3 HexString
  setCEO' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetCEOFn)

type SetCFOFn = Tagged (Proxy "setCFO(address)") (Tuple1 (Tagged (Proxy "_newCFO") Address))

setCFO :: TransactionOptions NoPay -> { _newCFO :: Address } -> Web3 HexString
setCFO x1 x2 = uncurryFields x2 $ setCFO' x1
  where
  setCFO' :: TransactionOptions NoPay -> Tagged (Proxy "_newCFO") Address -> Web3 HexString
  setCFO' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetCFOFn)

type SetCOOFn = Tagged (Proxy "setCOO(address)") (Tuple1 (Tagged (Proxy "_newCOO") Address))

setCOO :: TransactionOptions NoPay -> { _newCOO :: Address } -> Web3 HexString
setCOO x1 x2 = uncurryFields x2 $ setCOO' x1
  where
  setCOO' :: TransactionOptions NoPay -> Tagged (Proxy "_newCOO") Address -> Web3 HexString
  setCOO' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetCOOFn)

type SetGeneScienceAddressFn = Tagged (Proxy "setGeneScienceAddress(address)")
  (Tuple1 (Tagged (Proxy "_address") Address))

setGeneScienceAddress :: TransactionOptions NoPay -> { _address :: Address } -> Web3 HexString
setGeneScienceAddress x1 x2 = uncurryFields x2 $ setGeneScienceAddress' x1
  where
  setGeneScienceAddress'
    :: TransactionOptions NoPay -> Tagged (Proxy "_address") Address -> Web3 HexString
  setGeneScienceAddress' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetGeneScienceAddressFn)

type SetMetadataAddressFn = Tagged (Proxy "setMetadataAddress(address)")
  (Tuple1 (Tagged (Proxy "_contractAddress") Address))

setMetadataAddress :: TransactionOptions NoPay -> { _contractAddress :: Address } -> Web3 HexString
setMetadataAddress x1 x2 = uncurryFields x2 $ setMetadataAddress' x1
  where
  setMetadataAddress'
    :: TransactionOptions NoPay -> Tagged (Proxy "_contractAddress") Address -> Web3 HexString
  setMetadataAddress' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetMetadataAddressFn)

type SetNewAddressFn = Tagged (Proxy "setNewAddress(address)")
  (Tuple1 (Tagged (Proxy "_v2Address") Address))

setNewAddress :: TransactionOptions NoPay -> { _v2Address :: Address } -> Web3 HexString
setNewAddress x1 x2 = uncurryFields x2 $ setNewAddress' x1
  where
  setNewAddress'
    :: TransactionOptions NoPay -> Tagged (Proxy "_v2Address") Address -> Web3 HexString
  setNewAddress' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetNewAddressFn)

type SetSaleAuctionAddressFn = Tagged (Proxy "setSaleAuctionAddress(address)")
  (Tuple1 (Tagged (Proxy "_address") Address))

setSaleAuctionAddress :: TransactionOptions NoPay -> { _address :: Address } -> Web3 HexString
setSaleAuctionAddress x1 x2 = uncurryFields x2 $ setSaleAuctionAddress' x1
  where
  setSaleAuctionAddress'
    :: TransactionOptions NoPay -> Tagged (Proxy "_address") Address -> Web3 HexString
  setSaleAuctionAddress' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetSaleAuctionAddressFn)

type SetSecondsPerBlockFn = Tagged (Proxy "setSecondsPerBlock(uint256)")
  (Tuple1 (Tagged (Proxy "secs") (UIntN (D2 :& D5 :& DOne D6))))

setSecondsPerBlock
  :: TransactionOptions NoPay -> { secs :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
setSecondsPerBlock x1 x2 = uncurryFields x2 $ setSecondsPerBlock' x1
  where
  setSecondsPerBlock'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "secs") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  setSecondsPerBlock' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetSecondsPerBlockFn)

type SetSiringAuctionAddressFn = Tagged (Proxy "setSiringAuctionAddress(address)")
  (Tuple1 (Tagged (Proxy "_address") Address))

setSiringAuctionAddress :: TransactionOptions NoPay -> { _address :: Address } -> Web3 HexString
setSiringAuctionAddress x1 x2 = uncurryFields x2 $ setSiringAuctionAddress' x1
  where
  setSiringAuctionAddress'
    :: TransactionOptions NoPay -> Tagged (Proxy "_address") Address -> Web3 HexString
  setSiringAuctionAddress' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetSiringAuctionAddressFn)

type SireAllowedToAddressFn = Tagged (Proxy "sireAllowedToAddress(uint256)")
  (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

sireAllowedToAddress
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError Address)
sireAllowedToAddress x1 x2 x3 = sireAllowedToAddress' x1 x2 x3
  where
  sireAllowedToAddress'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError Address)
  sireAllowedToAddress' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: SireAllowedToAddressFn)

type SiringAuctionFn = Tagged (Proxy "siringAuction()") Tuple0

siringAuction :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
siringAuction x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: SiringAuctionFn)

type SupportsInterfaceFn = Tagged (Proxy "supportsInterface(bytes4)")
  (Tuple1 (Tagged (Proxy "_interfaceID") (BytesN (DOne D4))))

supportsInterface
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _interfaceID :: BytesN (DOne D4) }
  -> Web3 (Either CallError Boolean)
supportsInterface x1 x2 x3 = uncurryFields x3 $ supportsInterface' x1 x2
  where
  supportsInterface'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_interfaceID") (BytesN (DOne D4))
    -> Web3 (Either CallError Boolean)
  supportsInterface' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: SupportsInterfaceFn)

type SymbolFn = Tagged (Proxy "symbol()") Tuple0

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: SymbolFn)

type TokenMetadataFn = Tagged (Proxy "tokenMetadata(uint256,string)")
  ( Tuple2 (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))
      (Tagged (Proxy "_preferredTransport") String)
  )

tokenMetadata
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _tokenId :: UIntN (D2 :& D5 :& DOne D6), _preferredTransport :: String }
  -> Web3 (Either CallError String)
tokenMetadata x1 x2 x3 = uncurryFields x3 $ tokenMetadata' x1 x2
  where
  tokenMetadata'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Tagged (Proxy "_preferredTransport") String
    -> Web3 (Either CallError String)
  tokenMetadata' _x1 _x2 _x3 _x4 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple2 _x3 _x4 :: TokenMetadataFn)

type TokensOfOwnerFn = Tagged (Proxy "tokensOfOwner(address)")
  (Tuple1 (Tagged (Proxy "_owner") Address))

tokensOfOwner
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _owner :: Address }
  -> Web3 (Either CallError (Array (UIntN (D2 :& D5 :& DOne D6))))
tokensOfOwner x1 x2 x3 = uncurryFields x3 $ tokensOfOwner' x1 x2
  where
  tokensOfOwner'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_owner") Address
    -> Web3 (Either CallError (Array (UIntN (D2 :& D5 :& DOne D6))))
  tokensOfOwner' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: TokensOfOwnerFn)

type TotalSupplyFn = Tagged (Proxy "totalSupply()") Tuple0

totalSupply
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalSupply x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: TotalSupplyFn)

type TransferFn = Tagged (Proxy "transfer(address,uint256)")
  (Tuple2 (Tagged (Proxy "_to") Address) (Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

transfer
  :: TransactionOptions NoPay
  -> { _to :: Address, _tokenId :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
transfer x1 x2 = uncurryFields x2 $ transfer' x1
  where
  transfer'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_to") Address
    -> Tagged (Proxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  transfer' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: TransferFn)

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

type UnpauseFn = Tagged (Proxy "unpause()") Tuple0

unpause :: TransactionOptions NoPay -> Web3 HexString
unpause x1 = sendTx x1 (tagged Tuple0 :: UnpauseFn)

type WithdrawAuctionBalancesFn = Tagged (Proxy "withdrawAuctionBalances()") Tuple0

withdrawAuctionBalances :: TransactionOptions NoPay -> Web3 HexString
withdrawAuctionBalances x1 = sendTx x1 (tagged Tuple0 :: WithdrawAuctionBalancesFn)

type WithdrawBalanceFn = Tagged (Proxy "withdrawBalance()") Tuple0

withdrawBalance :: TransactionOptions NoPay -> Web3 HexString
withdrawBalance x1 = sendTx x1 (tagged Tuple0 :: WithdrawBalanceFn)
