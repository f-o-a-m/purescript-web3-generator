module Contracts.ReceiveImplemented where

import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, UIntN, Tuple1(..))
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)

type SimpleFunctionFn = Tagged (Proxy "simpleFunction(uint256)")
  (Tuple1 (Tagged (Proxy "x") (UIntN (D2 :& D5 :& DOne D6))))

simpleFunction
  :: TransactionOptions NoPay -> { x :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
simpleFunction x1 x2 = uncurryFields x2 $ simpleFunction' x1
  where
  simpleFunction'
    :: TransactionOptions NoPay -> Tagged (Proxy "x") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 HexString
  simpleFunction' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SimpleFunctionFn)
