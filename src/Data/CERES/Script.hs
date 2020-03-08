module Data.CERES.Script where


import qualified Data.Text                     as T

import           TextShow

import           Data.CERES.Type
import           Data.CERES.Value


type VPosition = VariablePosition VariablePlace

data VariablePlace
  = AtWorld Int
  | AtTime Int
  | AtDict
  | AtVar
  | AtLocal
  | AtCache
  | AtHere
  deriving (Eq, Ord, Show, Read)

instance TextShow VariablePlace where
  showb (AtWorld tIdx) =
    fromLazyText "AtWorld[" <> showb tIdx <> fromLazyText "]"
  showb AtDict = fromLazyText "AtDict"
  showb AtVar  = fromLazyText "AtVar"
  showb (AtTime tIdx) =
    fromLazyText "AtTime[" <> showb tIdx <> fromLazyText "]"
  showb AtLocal = fromLazyText "AtLocal"
  showb AtCache = fromLazyText "AtCache"
  showb AtHere  = fromLazyText "AtHere"

-- TODO: Would be Pseudo-Tree form
type CEREScript = [CERES]

data CERES
  -- | Initialize Variable at VPosition A as VPosition B
  = CRSInitVariable     VPosition VPosition
  -- | Set Value at VPosition A as VPosition B
  | CRSSetValue         VPosition VPosition
  -- | Delete Variable at VPosition A
  | CRSDeleteVariable   VPosition
  -- | Modify Value at VPosition A by CERESOperator with Value at VPosition B
  | CRSModifyValue      VPosition VPosition CERESOperator
  -- | Copy Value at VPosition B to Variable at VPosition A
  | CRSCopyValue        VPosition VPosition
  -- | Convert type of Value at VPosition A as like as Value at VPosition B
  | CRSConvertValue     VPosition ValueType
  -- | Convert type of Value at VPosition A as like as Value at VPosition B
  | CRSConvertValueBy VPosition VPosition
  -- TODO: ConvertValueTo
  deriving (Eq, Ord, Show, Read)

data CERESOperator
  = COAAdd
  | COAMul
  | COASub
  | COADiv
  | COAMod
  deriving (Eq, Ord, Show, Read)

instance TextShow CERESOperator where
  showb COAAdd = fromLazyText "COAAdd"
  showb COASub = fromLazyText "COASub"
  showb COAMul = fromLazyText "COAMul"
  showb COADiv = fromLazyText "COADiv"
  showb COAMod = fromLazyText "COAMod"
