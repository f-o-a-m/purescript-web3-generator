module Contracts.Adaption where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (Vector, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D0, D1, D2, D3, D4, D5, D6, D7, D8, DOne, UIntN, Tuple0(..), Tuple1(..), unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)

type AdoptFn = Tagged (Proxy "adopt(uint256)")
  (Tuple1 (Tagged (Proxy "petId") (UIntN (D2 :& D5 :& DOne D6))))

adopt :: TransactionOptions NoPay -> { petId :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
adopt x1 x2 = uncurryFields x2 $ adopt' x1
  where
  adopt'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "petId") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  adopt' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: AdoptFn)

type AdoptersFn = Tagged (Proxy "adopters(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

adopters
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError Address)
adopters x1 x2 x3 = adopters' x1 x2 x3
  where
  adopters'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError Address)
  adopters' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: AdoptersFn)

type GetAdoptersManyDigitFn = Tagged (Proxy "getAdoptersManyDigit()") Tuple0

getAdoptersManyDigit
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3
       ( Either CallError
           (Vector (D2 :& D1 :& D4 :& D7 :& D4 :& D8 :& D3 :& D6 :& D4 :& DOne D7) Address)
       )
getAdoptersManyDigit x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: GetAdoptersManyDigitFn)

type GetAdoptersOneDigitFn = Tagged (Proxy "getAdoptersOneDigit()") Tuple0

getAdoptersOneDigit
  :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Vector (DOne D1) Address))
getAdoptersOneDigit x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: GetAdoptersOneDigitFn)

type GetAdoptersTwoDigitFn = Tagged (Proxy "getAdoptersTwoDigit()") Tuple0

getAdoptersTwoDigit
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (Vector (D1 :& DOne D0) Address))
getAdoptersTwoDigit x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: GetAdoptersTwoDigitFn)
