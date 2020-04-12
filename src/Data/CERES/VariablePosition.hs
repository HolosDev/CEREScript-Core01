module Data.CERES.VariablePosition where


import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow

import           Data.CERES.Type
import           Data.CERES.Util
import           Data.CERES.Value


-- Variable Position for abstract variable's real place
data VariablePosition
  = VP VariablePlace VariableIndex
  deriving (Eq, Ord)

instance Show VariablePosition where
  show = TL.unpack . showtl

instance TextShow VariablePosition where
  showb (VP vPlace vIndex) =
    showb vPlace <> singleton '[' <> showb vIndex <> singleton ']'

-- TODO: Need to implement Eq/Ord instances for AtWorld and AtTime based on worldTime
-- instance Eq VariablePosition
-- instance Ord VariablePosition


data VariablePlace = AtTricky | AtWorld | AtTime | AtNWorld | AtNTime | AtDict | AtNDict | AtVars | AtNVars | AtLVars | AtLNVars | AtLTemp | AtLNTemp | AtHere | AtNull deriving (Eq, Ord, Enum, Bounded, Read)

instance Show VariablePlace where
  show = TL.unpack . showtl

instance TextShow VariablePlace where
  showb AtTricky = fromLazyText "AtTricky"
  showb AtWorld  = fromLazyText "AtWorld"
  showb AtTime   = fromLazyText "AtTime"
  showb AtNWorld = fromLazyText "AtNWorld"
  showb AtNTime  = fromLazyText "AtNTime"
  showb AtDict   = fromLazyText "AtDict"
  showb AtNDict  = fromLazyText "AtNDict"
  showb AtVars   = fromLazyText "AtVars"
  showb AtNVars  = fromLazyText "AtNVars"
  showb AtLVars  = fromLazyText "AtLVars"
  showb AtLNVars = fromLazyText "AtLNVars"
  showb AtLTemp  = fromLazyText "AtLTemp"
  showb AtLNTemp = fromLazyText "AtLNTemp"
  showb AtHere   = fromLazyText "AtHere"
  showb AtNull   = fromLazyText "AtNull"


data VariableIndex
  = VII Idx | VIN NKey | VIIT Idx Time | VINT NKey Time
  | VIIRI Idx [Idx] | VINRI NKey [Idx] | VIIRIT Idx [Idx] Time | VINRIT NKey [Idx] Time
  | VIV Value | VIAtom | VINull
  | PVII Idx | PVIN NKey | PVIT Time
  | PVIIRI Idx [Idx] | PVINRI NKey [Idx] | PVIIRIT Idx [Idx] Time | PVINRIT NKey [Idx] Time
  deriving (Eq, Ord)

instance Show VariableIndex where
  show = TL.unpack . showtl

instance TextShow VariableIndex where
  showb (VII idx                 )  = showb1 "VII" idx
  showb (VIN nKey                )  = showb1 "VIN" nKey
  showb (VIIT  idx  time         )  = showb2 "VIIT" idx time
  showb (VINT  nKey time         )  = showb2 "VINT" nKey time
  showb (VIIRI idx  indices      )  = showb2 "VIIRI" idx indices
  showb (VINRI nKey indices      )  = showb2 "VINRI" nKey indices
  showb (VIIRIT idx  indices time)  = showb3 "VIIRIT" idx indices time
  showb (VINRIT nKey indices time)  = showb3 "VINRIT" nKey indices time
  showb (VIV value               )  = showb1 "VIV" value
  showb VIAtom                      = fromLazyText "VIAtom"
  showb VINull                      = fromLazyText "VINull"
  showb (PVII idx                 ) = showb1 "PVII" idx
  showb (PVIN nKey                ) = showb1 "PVIN" nKey
  showb (PVIT time                ) = showb1 "PVIT" time
  showb (PVIIRI idx  indices      ) = showb2 "PVIIRI" idx indices
  showb (PVINRI nKey indices      ) = showb2 "PVINRI" nKey indices
  showb (PVIIRIT idx  indices time) = showb3 "PVIIRIT" idx indices time
  showb (PVINRIT nKey indices time) = showb3 "PVINRIT" nKey indices time
