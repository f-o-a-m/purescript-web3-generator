module Contracts.WeirdTypeStorage where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (Vector, call, sendTx)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)

type GetTypesFn = Tagged (Proxy "getTypes()") Tuple0

getTypes
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3
       ( Either CallError
           (Tuple2 (BytesN (D3 :& DOne D2)) (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))))
       )
getTypes x1 x2 = call x1 x2 (tagged Tuple0 :: GetTypesFn)

type NameFn = Tagged (Proxy "name()") Tuple0

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
name x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: NameFn)

type NumbersFn = Tagged (Proxy "numbers(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

numbers
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
numbers x1 x2 x3 = numbers' x1 x2 x3
  where
  numbers'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  numbers' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: NumbersFn)

type SetTypesFn = Tagged (Proxy "setTypes(bytes32,uint256[2])")
  (Tuple2 (BytesN (D3 :& DOne D2)) (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))))

setTypes
  :: TransactionOptions NoPay
  -> BytesN (D3 :& DOne D2)
  -> Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))
  -> Web3 HexString
setTypes x1 x2 x3 = setTypes' x1 x2 x3
  where
  setTypes'
    :: TransactionOptions NoPay
    -> BytesN (D3 :& DOne D2)
    -> Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  setTypes' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: SetTypesFn)
