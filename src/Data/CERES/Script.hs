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
  -- | ToInterpreter1 passes one Value at VPosition A
  | CRSToInterpreter1   VPosition
  -- | ToInterpreter2 passes one Value at VPosition A and VPosition B
  | CRSToInterpreter2   VPosition VPosition
  -- | ToInterpreter3 passes one Value at VPosition A, VPosition B and VPosition C
  | CRSToInterpreter3   VPosition VPosition VPosition
  -- | ToInterpreter4 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D
  | CRSToInterpreter4   VPosition VPosition VPosition VPosition
  -- | ToInterpreter5 passes one Value at VPosition A, VPosition B, VPosition C, VPosition D and VPosition E
  | CRSToInterpreter5   VPosition VPosition VPosition VPosition VPosition
  -- | ToInterpreter6 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E and VPosition F
  | CRSToInterpreter6   VPosition VPosition VPosition VPosition VPosition VPosition
  -- | ToInterpreter7 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F and VPosition G
  | CRSToInterpreter7   VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | ToInterpreter8 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F, VPosition G and VPosition H
  | CRSToInterpreter8   VPosition VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | Extend1 passes one Value at VPosition A
  | CRSExtend1   VPosition
  -- | Extend2 passes one Value at VPosition A and VPosition B
  | CRSExtend2   VPosition VPosition
  -- | Extend3 passes one Value at VPosition A, VPosition B and VPosition C
  | CRSExtend3   VPosition VPosition VPosition
  -- | Extend4 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D
  | CRSExtend4   VPosition VPosition VPosition VPosition
  -- | Extend5 passes one Value at VPosition A, VPosition B, VPosition C, VPosition D and VPosition E
  | CRSExtend5   VPosition VPosition VPosition VPosition VPosition
  -- | Extend6 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E and VPosition F
  | CRSExtend6   VPosition VPosition VPosition VPosition VPosition VPosition
  -- | Extend7 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F and VPosition G
  | CRSExtend7   VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | Extend8 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F, VPosition G and VPosition H
  | CRSExtend8   VPosition VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | No-Op
  | CRSNoop
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
  showb (CRSElapseTime vpA vpB        ) = showbCS2 "ElapseTime" vpA vpB
  showb (CRSSPControl vp              ) = showbCS1 "SPControl" vp
  showb (CRSSIControl vpA vpB         ) = showbCS2 "SIControl" vpA vpB
  showb (CRSSIInit vpA vpB vpC        ) = showbCS3 "SIInit" vpA vpB vpC
  showb (CRSToInterpreter1 vpA        ) = showbCS1 "ToInterpreter1" vpA
  showb (CRSToInterpreter2 vpA vpB    ) = showbCS2 "ToInterpreter2" vpA vpB
  showb (CRSToInterpreter3 vpA vpB vpC) = showbCS3 "ToInterpreter3" vpA vpB vpC
  showb (CRSToInterpreter4 vpA vpB vpC vpD) =
    showbCS4 "ToInterpreter4" vpA vpB vpC vpD
  showb (CRSToInterpreter5 vpA vpB vpC vpD vpE) =
    showbCS5 "ToInterpreter5" vpA vpB vpC vpD vpE
  showb (CRSToInterpreter6 vpA vpB vpC vpD vpE vpF) =
    showbCS6 "ToInterpreter6" vpA vpB vpC vpD vpE vpF
  showb (CRSToInterpreter7 vpA vpB vpC vpD vpE vpF vpG) =
    showbCS7 "ToInterpreter7" vpA vpB vpC vpD vpE vpF vpG
  showb (CRSToInterpreter8 vpA vpB vpC vpD vpE vpF vpG vpH) =
    showbCS8 "ToInterpreter8" vpA vpB vpC vpD vpE vpF vpG vpH
  showb (CRSExtend1 vpA            ) = showbCS1 "Extend1" vpA
  showb (CRSExtend2 vpA vpB        ) = showbCS2 "Extend2" vpA vpB
  showb (CRSExtend3 vpA vpB vpC    ) = showbCS3 "Extend3" vpA vpB vpC
  showb (CRSExtend4 vpA vpB vpC vpD) = showbCS4 "Extend4" vpA vpB vpC vpD
  showb (CRSExtend5 vpA vpB vpC vpD vpE) =
    showbCS5 "Extend5" vpA vpB vpC vpD vpE
  showb (CRSExtend6 vpA vpB vpC vpD vpE vpF) =
    showbCS6 "Extend6" vpA vpB vpC vpD vpE vpF
  showb (CRSExtend7 vpA vpB vpC vpD vpE vpF vpG) =
    showbCS7 "Extend7" vpA vpB vpC vpD vpE vpF vpG
  showb (CRSExtend8 vpA vpB vpC vpD vpE vpF vpG vpH) =
    showbCS8 "Extend8" vpA vpB vpC vpD vpE vpF vpG vpH
  showb CRSNoop = fromLazyText "Noop"


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
  show = TL.unpack . showtl

instance TextShow CERESOperator where
  showb COAAdd = fromLazyText "Add"
  showb COASub = fromLazyText "Sub"
  showb COAMul = fromLazyText "Mul"
  showb COADiv = fromLazyText "Div"
  showb COAMod = fromLazyText "Mod"
  showb CORSwp = fromLazyText "Swp"
  showb CORMov = fromLazyText "Mov"
