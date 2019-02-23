module Data.CERES.Value where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Type


data ValueTyper = ValueTyper
  { valueTyperName :: Name
  , valueType :: ValueType
  } deriving (Show, Eq)

data Value
  = IntValue { iV :: Int }
  | DblValue { dV :: Double }
  | StrValue { sV :: Text }
  | BoolValue { bV :: Bool }
  deriving (Eq, Ord)

instance Show Value where
  show (IntValue iV)  = "IV<| " ++ show iV ++ " |>"
  show (DblValue dV)  = "DV<| " ++ show dV ++ " |>"
  show (StrValue sV)  = "SV<\"" ++ T.unpack sV ++ "\">"
  show (BoolValue bV) = "BV<| " ++ show bV ++ " |>"

data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  deriving (Eq, Ord, Enum)

instance Show ValueType where
  show VTInt  = "C-Int"
  show VTDbl  = "C-Dbl"
  show VTStr  = "C-Str"
  show VTBool = "CBool"
