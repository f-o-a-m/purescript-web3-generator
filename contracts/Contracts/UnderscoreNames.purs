--------------------------------------------------------------------------------
-- | UnderscoreNames
--------------------------------------------------------------------------------

module Contracts.UnderscoreNames where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Solidity (Tuple0(..))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)
--------------------------------------------------------------------------------
-- | FnT_functionWithUnderscoreFn
--------------------------------------------------------------------------------


type FnT_functionWithUnderscoreFn = Tagged (Proxy "_functionWithUnderscore()") (Tuple0 )

_functionWithUnderscore :: TransactionOptions NoPay -> Web3 HexString
_functionWithUnderscore x0 = sendTx x0 ((tagged $ Tuple0 ) :: FnT_functionWithUnderscoreFn)