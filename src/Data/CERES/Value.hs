module Data.CERES.Value where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Data.CERES.Type


data ValueTyper = ValueTyper
  { valueTyperName :: Name
  , valueType      :: ValueType
  } deriving (Show, Eq)

data ValueContainer = VC
  { value     :: Value
  , valueInfo :: ValueInfo
  } deriving (Show, Eq)

-- TODO: Not yet Implemented
data ValueInfo = ValueInfo
  { valueEdited       :: Bool
  , valueDependencies :: [Branch]
  } deriving (Show, Eq)

-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
data Value
  = IntValue { iV :: Int }
  | DblValue { dV :: Double }
  | StrValue { sV :: Text }
  | BoolValue { bV :: Bool }
  | ErrValue { errMessage :: Message }
  deriving (Eq, Ord, Read)

instance Show Value where
  show (IntValue  iV) = "IV<| " ++ show iV ++ " |>"
  show (DblValue  dV) = "DV<| " ++ show dV ++ " |>"
  show (StrValue  sV) = "SV<\"" ++ T.unpack sV ++ "\">"
  show (BoolValue bV) = "BV<| " ++ show bV ++ " |>"
  show (ErrValue  bV) = "EV<| " ++ T.unpack bV ++ " |>"


data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  | VTErr
  deriving (Eq, Ord, Enum, Read)

instance Show ValueType where
  show VTInt  = "C-Int"
  show VTDbl  = "C-Dbl"
  show VTStr  = "C-Str"
  show VTBool = "CBool"
  show VTErr  = "C-Err"

-- Variable Position for abstract variable's real place
data VariablePosition vp = VP
  { variableID    :: ID
  , variablePlace :: vp -- VariablePlace
  -- TODO: Need to think that the type of `hereValue` should be `Maybe Value` or just `Value`
  -- TODO: Or introduce a new Data Constructor `VPH` for `VariablePosition`
  , hereValue     :: Value -- Not ValueContainer or etc.
  } deriving (Eq, Ord, Read)

voidHere :: Value
voidHere = ErrValue "Void AtHere"

instance Show vp => Show (VariablePosition vp) where
  show vc =
    "<[" ++ show (variableID vc) ++ "@" ++ show (variablePlace vc) ++ "]>"
