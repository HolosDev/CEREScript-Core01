module Data.CERES.Value where

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow

import           Data.CERES.Type


type ValueMap = IntMap Value

blankVM = IM.empty

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
  | AtomValue
  | ErrValue { errMessage :: Message }
  deriving (Eq, Ord, Read)

instance Show Value where
  show = TL.unpack . showtl

showRaw :: Value -> String
showRaw (IntValue  i) = show i
showRaw (DblValue  d) = show d
showRaw (StrValue  s) = TL.unpack s
showRaw (BoolValue b) = show b
showRaw AtomValue     = "Atom"
showRaw (ErrValue e)  = TL.unpack e

instance TextShow Value where
  showb (IntValue i) = fromLazyText "IV<| " <> showb i <> fromLazyText " |>"
  showb (DblValue d) = fromLazyText "DV<| " <> showb d <> fromLazyText " |>"
  showb (StrValue s) =
    fromLazyText "SV<\"" <> fromLazyText s <> fromLazyText "\">"
  showb (BoolValue b) = fromLazyText "BV<| " <> showb b <> fromLazyText " |>"
  showb AtomValue     = fromLazyText "AV<|  |>"
  showb (ErrValue e) =
    fromLazyText "EV<| " <> fromLazyText e <> fromLazyText " |>"

showRawTL :: Value -> Text
showRawTL (IntValue  i) = showtl i
showRawTL (DblValue  d) = showtl d
showRawTL (StrValue  s) = s
showRawTL (BoolValue b) = showtl b
showRawTL AtomValue     = "Atom"
showRawTL (ErrValue e)  = e


data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  | VTAtom
  | VTErr
  deriving (Eq, Ord, Enum, Read)

instance Show ValueType where
  show VTInt  = "C-Int"
  show VTDbl  = "C-Dbl"
  show VTStr  = "C-Str"
  show VTBool = "CBool"
  show VTAtom = "CAtom"
  show VTErr  = "C-Err"

instance TextShow ValueType where
  showb VTInt  = fromLazyText "C-Int"
  showb VTDbl  = fromLazyText "C-Dbl"
  showb VTStr  = fromLazyText "C-Str"
  showb VTBool = fromLazyText "CBool"
  showb VTAtom = fromLazyText "CAtom"
  showb VTErr  = fromLazyText "C-Err"

-- Variable Position for abstract variable's real place
data VariablePosition vp = VP
  { variableID    :: ID
  , variablePlace :: vp -- VariablePlace
  -- TODO: Need to think that the type of `hereValue` should be `Maybe Value` or just `Value`
  -- TODO: Or introduce a new Data Constructor `VPH` for `VariablePosition`
  , hereValue     :: Value -- Not ValueContainer or etc.
  } deriving (Eq, Read)

instance Ord vp => Ord (VariablePosition vp) where
  compare (VP idxA vpA hvA) (VP idxB vpB hvB) = if vpA == vpB
    then if idxA == idxB then compare hvA hvB else compare idxA idxB
    else compare vpA vpB

voidHere :: Value
voidHere = ErrValue "Void AtHere"

instance (Show vp, TextShow vp) => Show (VariablePosition vp) where
  show = TL.unpack . showtl

instance TextShow vp => TextShow (VariablePosition vp) where
  showb vc =
    fromLazyText "<["
      <> showb (variableID vc)
      <> fromLazyText "@"
      <> showb (variablePlace vc)
      <> fromLazyText "]>"
