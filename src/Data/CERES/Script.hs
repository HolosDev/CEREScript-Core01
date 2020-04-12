module Data.CERES.Script where


import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           TextShow

import           Data.CERES.Type
import           Data.CERES.Util
import           Data.CERES.Value
import           Data.CERES.VariablePosition


type VPosition = VariablePosition

-- TODO: Would be Pseudo-Tree form
type CEREScript = [CERES]

data CERES
  -- | Initialize Variable VPosition A with Value at VPosition B
  = CRSInitVariable     VPosition VPosition
  -- | Initialize Variable at position which stored in VPosition A with Value at VPosition B
  | CRSInitVariableAt   VPosition VPosition
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
  | CRSConvertValueBy   VPosition VPosition
  -- | Convert value at VPosition A with a given rule VPosition B
  | CRSConvertValueWith VPosition VPosition
  -- | Replace StrValue at VPosition A with indicated Value in the StrValue
  | CRSReplaceText      VPosition
  -- | Replace StrValue at VPosition A with indicated Value in the StrValue to VPosition B
  | CRSReplaceTextTo    VPosition VPosition
  -- | Generate Random Value at VPosition A as a ValueType
  | CRSRandom           VPosition ValueType
  -- | Generate Random Value at VPosition A as a type VPosition B
  | CRSRandomBy         VPosition VPosition
  -- | Generate Random Value at VPosition A as a ValueType, And parameters vpC, vpD, and vpE
  | CRSRandomWith       VPosition ValueType VPosition VPosition VPosition
  -- | Generate Random Value at VPosition A as a type VPosition B, And parameters vpC, vpD, and vpE
  | CRSRandomWithBy     VPosition VPosition VPosition VPosition VPosition
  -- | ElapseTime <{Absolute,Scale}> <ScaleSize>
  | CRSElapseTime       VPosition VPosition
  -- | SPControl <{Stop,Pause}>
  | CRSSPControl        VPosition
  -- | SIControl <{Retain,Forget,Init,Abolish}> <JumpOffset>
  | CRSSIControl        VPosition VPosition
  -- | SIInit <SpoolID> <Given SIName> <where initiated SI ID store>
  | CRSSIInit           VPosition VPosition VPosition
  deriving (Eq, Ord)

instance Show CERES where
  show = TL.unpack . showtl

instance TextShow CERES where
  showb (CRSInitVariable   vpA vpB    ) = showbCS2 "InitVariable" vpA vpB
  showb (CRSInitVariableAt vpA vpB    ) = showbCS2 "InitVariableAt" vpA vpB
  showb (CRSSetValue       vpA vpB    ) = showbCS2 "SetValue" vpA vpB
  showb (CRSDeleteVariable vp         ) = showbCS1 "DeleteVariable" vp
  showb (CRSModifyValue vpA vpB cOper ) = showbCS3 "ModifyValue" vpA vpB cOper
  showb (CRSCopyValue        vpA vpB  ) = showbCS2 "CopyValue" vpA vpB
  showb (CRSConvertValue     vp  vType) = showbCS2 "ConvertValue" vp vType
  showb (CRSConvertValueBy   vpA vpB  ) = showbCS2 "ConvertValueBy" vpA vpB
  showb (CRSConvertValueWith vpA vpB  ) = showbCS2 "ConvertValueWith" vpA vpB
  showb (CRSReplaceText vp            ) = showbCS1 "ReplaceText" vp
  showb (CRSReplaceTextTo vpA vpB     ) = showbCS2 "ReplaceTextTo" vpA vpB
  showb (CRSRandom        vp  vType   ) = showbCS2 "Random" vp vType
  showb (CRSRandomBy      vpA vpB     ) = showbCS2 "RandomBy" vpA vpB
  showb (CRSRandomWith vpA vtB vpC vpD vpE) =
    showbCS5 "RandomWith" vpA vtB vpC vpD vpE
  showb (CRSRandomWithBy vpA vpB vpC vpD vpE) =
    showbCS5 "RandomWithBy" vpA vpB vpC vpD vpE
  showb (CRSElapseTime vpA vpB) = showbCS2 "ElapseTime" vpA vpB
  showb (CRSSPControl vp      ) = showbCS1 "SPControl" vp
  showb (CRSSIControl vpA vpB ) = showbCS2 "SIControl" vpA vpB
  showb (CRSSIInit vpA vpB vpC) = showbCS3 "SIInit" vpA vpB vpC


data CERESOperator
  = COAAdd
  | COAMul
  | COASub
  | COADiv
  | COAMod
  deriving (Eq, Ord, Enum)

instance Show CERESOperator where
  show = TL.unpack . showtl

instance TextShow CERESOperator where
  showb COAAdd = fromLazyText "Add"
  showb COASub = fromLazyText "Sub"
  showb COAMul = fromLazyText "Mul"
  showb COADiv = fromLazyText "Div"
  showb COAMod = fromLazyText "Mod"
