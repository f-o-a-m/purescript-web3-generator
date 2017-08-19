module Data.Generator where

import Prelude
import Data.String (joinWith, take, drop, toUpper)
import Data.Array (length)
import Data.Monoid (mempty)
import Data.AbiParser (SolidityType(..), SolidityFunction(..), format)

import Network.Ethereum.Web3.Types (HexString(..), unHex, sha3)

--------------------------------------------------------------------------------
class Code a where
  genCode :: a -> String

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------
toSelector :: SolidityFunction -> HexString
toSelector (SolidityFunction f) =
  let args = map (\i -> format i) f.inputs
      HexString hx = sha3 $ f.name <> "(" <> joinWith "," args <> ")"
  in HexString $ take 8 hx

capitalize :: String -> String
capitalize s =
  let h = toUpper $ take 1 s
      rest = drop 1 s
  in h <> rest

byteSizeDigits :: Int -> String
byteSizeDigits n =
  let ones = n `mod` 10
      tens = n - ones `div` 10
  in if tens == 0
       then "D" <> show ones
       else "(" <> "D" <> show tens <> " :& " <> "D" <> show ones <> ")"

vectorLength :: Int -> String
vectorLength n = "N" <> show n

toPSType :: SolidityType -> String
toPSType s = case s of
  SolidityBool -> "Boolean"
  SolidityUint -> "BigNumber"
  SolidityString -> "String"
  SolidityBytesN n -> "BytesN " <> byteSizeDigits n
  SolidityBytesD -> "ByteString"
  SolidityVector n a -> "Vector " <> vectorLength n <> " " <> toPSType a
  SolidityArray a -> "Array " <> toPSType a

--------------------------------------------------------------------------------
-- | Data decleration and instances
--------------------------------------------------------------------------------

data DataDecl =
  DataDecl { constructor :: String
           , argTypes :: Array String
           }

instance codeDataDecl :: Code DataDecl where
  genCode (DataDecl decl) =
    "data " <> decl.constructor <> " = " <> decl.constructor <> " " <> joinWith " " decl.argTypes

funToDataDecl :: SolidityFunction -> DataDecl
funToDataDecl (SolidityFunction f) =
  DataDecl { constructor : capitalize f.name
           , argTypes : map toPSType f.inputs
           }

data BuilderMethod =
  BuilderMethod { unpackExpr :: String
                , builderExpr :: String
                }

funToBuilder :: SolidityFunction -> BuilderMethod
funToBuilder fun@(SolidityFunction f) =
  let selectorBuilder = "HexString " <> (unHex $ toSelector fun)
      sep = " <> toDataBuilder "
      restBuilder = if length f.inputs == 0 then mempty else " <> " <> joinWith sep (map toPSType f.inputs)
  in BuilderMethod { unpackExpr : capitalize f.name <> " " <> "f"
                   , builderExpr : selectorBuilder <> restBuilder
                   }

data AbiEncodingInstance =
  AbiEncodingInstance { instanceType :: String
                      , instanceName :: String
                      , builder :: String
                      , parser :: String
                      }

funToEncodingInstance :: SolidityFunction -> AbiEncodingInstance
funToEncodingInstance fun@(SolidityFunction f) =
  let BuilderMethod m = funToBuilder fun
  in  AbiEncodingInstance { instanceType : capitalize f.name
                          , instanceName : "abiEncoding" <> capitalize f.name
                          , builder : "toDataBuilder " <> m.unpackExpr <> " = " <> m.builderExpr
                          , parser : "fromDataParser = fail \"Function type has no parser.\""
                          }

instance codeAbiEncodingInstance :: Code AbiEncodingInstance where
  genCode (AbiEncodingInstance i) =
    let header = "instance " <> i.instanceName <> " :: ABIEncoding " <> i.instanceType <> " where"
        bldr = "\t" <> i.builder
        prsr = "\t" <> i.parser
    in joinWith "\n" [header, bldr, prsr]
