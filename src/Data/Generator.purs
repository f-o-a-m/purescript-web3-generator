module Data.Generator where

import Prelude
import Data.String (joinWith, take, drop, toUpper)
import Data.AbiParser (SolidityFunction(..), format)

import Network.Ethereum.Web3.Types (HexString, sha3)

toSelector :: SolidityFunction -> HexString
toSelector (SolidityFunction f) =
  let args = map (\i -> format i) f.inputs
  in sha3 $ f.name <> "(" <> joinWith "," args <> ")"

capitalize :: String -> String
capitalize s =
  let h = toUpper $ take 1 s
      rest = drop 1 s
  in h <> rest
