module Data.CERES.Operator where


import qualified Data.Text.Lazy                as TL

import           TextShow


data CERESOperator
  = COAAdd
  | COAMul
  | COASub
  | COADiv
  | COAMod
  | CORSwp
  | CORMov
  deriving (Eq, Ord, Enum)

instance Show CERESOperator where
  show = toString . showb

instance TextShow CERESOperator where
  showb COAAdd = fromLazyText "Add"
  showb COASub = fromLazyText "Sub"
  showb COAMul = fromLazyText "Mul"
  showb COADiv = fromLazyText "Div"
  showb COAMod = fromLazyText "Mod"
  showb CORSwp = fromLazyText "Swp"
  showb CORMov = fromLazyText "Mov"
