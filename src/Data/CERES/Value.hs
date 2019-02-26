module Data.CERES.Value where

import           Data.Text (Text)
import qualified Data.Text as T

import Data.CERES.Type


data ValueTyper = ValueTyper
  { valueTyperName :: Name
  , valueType      :: ValueType
  } deriving (Show, Eq)

data Value
  = IntValue { iV :: Int }
  | DblValue { dV :: Double }
  | StrValue { sV :: Text }
  | BoolValue { bV :: Bool }
  | ErrValue { errMessage :: Message }
  deriving (Eq, Ord)

instance Show Value where
  show (IntValue iV)  = "IV<| " ++ show iV ++ " |>"
  show (DblValue dV)  = "DV<| " ++ show dV ++ " |>"
  show (StrValue sV)  = "SV<\"" ++ T.unpack sV ++ "\">"
  show (BoolValue bV) = "BV<| " ++ show bV ++ " |>"
  show (ErrValue bV) = "EV<| " ++ T.unpack bV ++ " |>"

data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  | VTErr
  deriving (Eq, Ord, Enum)

instance Show ValueType where
  show VTInt  = "C-Int"
  show VTDbl  = "C-Dbl"
  show VTStr  = "C-Str"
  show VTBool = "CBool"
  show VTErr  = "C-Err"
