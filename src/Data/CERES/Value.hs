module Data.CERES.Value where

import           Data.Text (Text)
import qualified Data.Text as T

import Data.CERES.Type


data ValueTyper = ValueTyper
  { valueTyperName :: Name
  , valueType      :: ValueType
  } deriving (Show, Eq)

-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
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

errValueWith2 :: (Show a, Show b) => Name -> Message -> a -> b -> Value
errValueWith2 funcName errorType vA vB = ErrValue errorMessage
  where
    errorMessage = T.concat
      [ "[Error]<"
      , funcName
      , " :=: "
      , errorType
      , "> "
      , T.pack . show $ vA
      , " and "
      , T.pack . show $ vB
      ]

errValueTEWith2 :: (Show a, Show b) => Name -> a -> b -> Value
errValueTEWith2 funcName = errValueWith2 funcName "TypeError"

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

-- Value Container for abstract vaule's real place and type
data ValueContainer vp = VC
  { valueID :: ID
  , valuePlace :: vp -- ValuePlace
  } deriving (Eq, Ord)

instance Show vp => Show (ValueContainer vp) where
  show vc = "<[" ++ show (valueID vc) ++ "@" ++ show (valuePlace vc) ++ "]>"

data StandardValuePlace
  = AtLocal
  | AtTime
  | AtWorld
  | AtDict
  | AtVar
  deriving (Eq, Ord, Enum,  Show)
