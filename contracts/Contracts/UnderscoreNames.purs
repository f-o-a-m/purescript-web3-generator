module Contracts.UnderscoreNames where

import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Solidity (Tuple0(..))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)

type FnT_functionWithUnderscoreFn = Tagged (Proxy "_functionWithUnderscore()") Tuple0

_functionWithUnderscore :: TransactionOptions NoPay -> Web3 HexString
_functionWithUnderscore x1 = sendTx x1 (tagged Tuple0 :: FnT_functionWithUnderscoreFn)
