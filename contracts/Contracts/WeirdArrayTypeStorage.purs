module Contracts.WeirdArrayTypeStorage where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)

type GetHashTypesFn = Tagged (Proxy "getHashTypes(bytes32[],string)")
  (Tuple2 (Array (BytesN (D3 :& DOne D2))) String)

getHashTypes
  :: TransactionOptions NoPay -> Array (BytesN (D3 :& DOne D2)) -> String -> Web3 HexString
getHashTypes x1 x2 x3 = getHashTypes' x1 x2 x3
  where
  getHashTypes'
    :: TransactionOptions NoPay -> Array (BytesN (D3 :& DOne D2)) -> String -> Web3 HexString
  getHashTypes' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: GetHashTypesFn)

type GetTypesFn = Tagged (Proxy "getTypes()") Tuple0

getTypes
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (Tuple2 (Array (BytesN (D3 :& DOne D2))) String))
getTypes x1 x2 = call x1 x2 (tagged Tuple0 :: GetTypesFn)

type HashesFn = Tagged (Proxy "hashes(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

hashes
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
hashes x1 x2 x3 = hashes' x1 x2 x3
  where
  hashes'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
  hashes' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: HashesFn)

type NumbersFn = Tagged (Proxy "numbers()") Tuple0

numbers :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
numbers x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: NumbersFn)
