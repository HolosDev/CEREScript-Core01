module Data.CERES.Data where


import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Trie.Text                 ( Trie )
import           Data.Trie.Text                as Trie

import           TextShow                      as TS

import           Data.CERES.Type
import           Data.CERES.Util
import           Data.CERES.Operator


-------------------------------- VariablePosition --------------------------------

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
  -- | Check existence of Variable at VPosition A and write the result to VPosition B
  | CRSCheckVariable   VPosition VPosition
  -- | Modify Value at VPosition A by CERESOperator
  | CRSModifyValue1     VPosition CERESOperator
  -- | Modify Value at VPosition A and Value at VPosition B by CERESOperator
  | CRSModifyValue2     VPosition VPosition CERESOperator
  -- | Modify Value at VPosition A and Value at VPosition B by CERESOperator and write the result to VPosition C
  | CRSModifyValue3     VPosition VPosition CERESOperator VPosition
  -- | Copy Value at VPosition B to Variable of VPosition A
  | CRSCopyValue        VPosition VPosition
  -- | Convert type of Value at VPosition A as like as Value at VPosition B
  | CRSConvertValue     VPosition ValueType
  -- | Convert type of Value at VPosition A as like as Value at VPosition B
  | CRSConvertValueBy   VPosition VPosition
  -- | Convert value at VPosition A with a given rule VPosition B
  | CRSConvertValueWith VPosition VPosition
  -- | Replace StrValue at VPosition A with indicated Variable in the StrValue
  | CRSReplaceText      VPosition
  -- | Replace StrValue at VPosition A with indicated Variable in the StrValue to VPosition B
  | CRSReplaceTextTo    VPosition VPosition
  -- | Read & Parse StrValue of VPosition A as a PtrValue and write to VPosition B
  | CRSGetVPosition     VPosition VPosition
  -- | Set PtrValue of VPosition A to VPosition B
  | CRSSetVPosition     VPosition VPosition
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
  -- | SIInit <SpoolID> <Given SIName> <where initiated SI ID store> <JumpOffset>
  | CRSSIInit           VPosition VPosition VPosition VPosition
  -- | End SI when Value at VPosition A is True. This would be deprecated when Control Flow is implemented.
  | CRSSIEnd            VPosition
  -- | No-Op
  | CRSNoop
  -- | Log to VPosition A with a content of VPosition B
  | CRSLog VPosition VPosition
  -- | Parse a script text of VPosition A and store the script to VPosition B
  | CRSParseScript VPosition VPosition
  -- | ToInterpreter0
  | CRSToInterpreter0 CHeader
  -- | ToInterpreter1 passes one Value at VPosition A
  | CRSToInterpreter1 CHeader VPosition
  -- | ToInterpreter2 passes one Value at VPosition A and VPosition B
  | CRSToInterpreter2 CHeader VPosition VPosition
  -- | ToInterpreter3 passes one Value at VPosition A, VPosition B and VPosition C
  | CRSToInterpreter3 CHeader VPosition VPosition VPosition
  -- | ToInterpreter4 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D
  | CRSToInterpreter4 CHeader VPosition VPosition VPosition VPosition
  -- | ToInterpreter5 passes one Value at VPosition A, VPosition B, VPosition C, VPosition D and VPosition E
  | CRSToInterpreter5 CHeader VPosition VPosition VPosition VPosition VPosition
  -- | ToInterpreter6 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E and VPosition F
  | CRSToInterpreter6 CHeader VPosition VPosition VPosition VPosition VPosition VPosition
  -- | ToInterpreter7 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F and VPosition G
  | CRSToInterpreter7 CHeader VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | ToInterpreter8 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F, VPosition G and VPosition H
  | CRSToInterpreter8 CHeader VPosition VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | Extend0
  | CRSExtend0 CHeader
  -- | Extend1 passes one Value at VPosition A
  | CRSExtend1 CHeader VPosition
  -- | Extend2 passes one Value at VPosition A and VPosition B
  | CRSExtend2 CHeader VPosition VPosition
  -- | Extend3 passes one Value at VPosition A, VPosition B and VPosition C
  | CRSExtend3 CHeader VPosition VPosition VPosition
  -- | Extend4 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D
  | CRSExtend4 CHeader VPosition VPosition VPosition VPosition
  -- | Extend5 passes one Value at VPosition A, VPosition B, VPosition C, VPosition D and VPosition E
  | CRSExtend5 CHeader VPosition VPosition VPosition VPosition VPosition
  -- | Extend6 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E and VPosition F
  | CRSExtend6 CHeader VPosition VPosition VPosition VPosition VPosition VPosition
  -- | Extend7 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F and VPosition G
  | CRSExtend7 CHeader VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  -- | Extend8 passes one Value at VPosition A, VPosition B, VPosition C and VPosition D, VPosition E, VPosition F, VPosition G and VPosition H
  | CRSExtend8 CHeader VPosition VPosition VPosition VPosition VPosition VPosition VPosition VPosition
  deriving (Eq, Ord)

instance Show CERES where
  show = toString . showb

instance TextShow CERES where
  showb (CRSInitVariable   vpA vpB    ) = showbCS2 "InitVariable" vpA vpB
  showb (CRSInitVariableAt vpA vpB    ) = showbCS2 "InitVariableAt" vpA vpB
  showb (CRSSetValue       vpA vpB    ) = showbCS2 "SetValue" vpA vpB
  showb (CRSDeleteVariable vp         ) = showbCS1 "DeleteVariable" vp
  showb (CRSCheckVariable vpA vpB     ) = showbCS2 "CheckVariable" vpA vpB
  showb (CRSModifyValue1  vpA cOper   ) = showbCS2 "ModifyValue1" vpA cOper
  showb (CRSModifyValue2 vpA vpB cOper) = showbCS3 "ModifyValue2" vpA vpB cOper
  showb (CRSModifyValue3 vpA vpB cOper vpC) =
    showbCS4 "ModifyValue3" vpA vpB cOper vpC
  showb (CRSCopyValue        vpA vpB  ) = showbCS2 "CopyValue" vpA vpB
  showb (CRSConvertValue     vp  vType) = showbCS2 "ConvertValue" vp vType
  showb (CRSConvertValueBy   vpA vpB  ) = showbCS2 "ConvertValueBy" vpA vpB
  showb (CRSConvertValueWith vpA vpB  ) = showbCS2 "ConvertValueWith" vpA vpB
  showb (CRSReplaceText vp            ) = showbCS1 "ReplaceText" vp
  showb (CRSReplaceTextTo vpA vpB     ) = showbCS2 "ReplaceTextTo" vpA vpB
  showb (CRSGetVPosition  vpA vpB     ) = showbCS2 "GetVPosition" vpA vpB
  showb (CRSSetVPosition  vpA vpB     ) = showbCS2 "SetVPosition" vpA vpB
  showb (CRSRandom        vp  vType   ) = showbCS2 "Random" vp vType
  showb (CRSRandomBy      vpA vpB     ) = showbCS2 "RandomBy" vpA vpB
  showb (CRSRandomWith vpA vtB vpC vpD vpE) =
    showbCS5 "RandomWith" vpA vtB vpC vpD vpE
  showb (CRSRandomWithBy vpA vpB vpC vpD vpE) =
    showbCS5 "RandomWithBy" vpA vpB vpC vpD vpE
  showb (CRSElapseTime vpA vpB    )    = showbCS2 "ElapseTime" vpA vpB
  showb (CRSSPControl vp          )    = showbCS1 "SPControl" vp
  showb (CRSSIControl vpA vpB     )    = showbCS2 "SIControl" vpA vpB
  showb (CRSSIInit vpA vpB vpC vpD)    = showbCS4 "SIInit" vpA vpB vpC vpD
  showb (CRSSIEnd vp              )    = showbCS1 "SIEnd" vp
  showb CRSNoop                        = fromText "Noop"
  showb (CRSLog         vpA vpB      ) = showbCS2 "Log" vpA vpB
  showb (CRSParseScript vpA vpB      ) = showbCS2 "ParseScript" vpA vpB
  showb (CRSToInterpreter0 cH        ) = showbCSC0 "ToInterpreter0" cH
  showb (CRSToInterpreter1 cH vpA    ) = showbCSC1 "ToInterpreter1" cH vpA
  showb (CRSToInterpreter2 cH vpA vpB) = showbCSC2 "ToInterpreter2" cH vpA vpB
  showb (CRSToInterpreter3 cH vpA vpB vpC) =
    showbCSC3 "ToInterpreter3" cH vpA vpB vpC
  showb (CRSToInterpreter4 cH vpA vpB vpC vpD) =
    showbCSC4 "ToInterpreter4" cH vpA vpB vpC vpD
  showb (CRSToInterpreter5 cH vpA vpB vpC vpD vpE) =
    showbCSC5 "ToInterpreter5" cH vpA vpB vpC vpD vpE
  showb (CRSToInterpreter6 cH vpA vpB vpC vpD vpE vpF) =
    showbCSC6 "ToInterpreter6" cH vpA vpB vpC vpD vpE vpF
  showb (CRSToInterpreter7 cH vpA vpB vpC vpD vpE vpF vpG) =
    showbCSC7 "ToInterpreter7" cH vpA vpB vpC vpD vpE vpF vpG
  showb (CRSToInterpreter8 cH vpA vpB vpC vpD vpE vpF vpG vpH) =
    showbCSC8 "ToInterpreter8" cH vpA vpB vpC vpD vpE vpF vpG vpH
  showb (CRSExtend0 cH            ) = showbCSC0 "Extend0" cH
  showb (CRSExtend1 cH vpA        ) = showbCSC1 "Extend1" cH vpA
  showb (CRSExtend2 cH vpA vpB    ) = showbCSC2 "Extend2" cH vpA vpB
  showb (CRSExtend3 cH vpA vpB vpC) = showbCSC3 "Extend3" cH vpA vpB vpC
  showb (CRSExtend4 cH vpA vpB vpC vpD) =
    showbCSC4 "Extend4" cH vpA vpB vpC vpD
  showb (CRSExtend5 cH vpA vpB vpC vpD vpE) =
    showbCSC5 "Extend5" cH vpA vpB vpC vpD vpE
  showb (CRSExtend6 cH vpA vpB vpC vpD vpE vpF) =
    showbCSC6 "Extend6" cH vpA vpB vpC vpD vpE vpF
  showb (CRSExtend7 cH vpA vpB vpC vpD vpE vpF vpG) =
    showbCSC7 "Extend7" cH vpA vpB vpC vpD vpE vpF vpG
  showb (CRSExtend8 cH vpA vpB vpC vpD vpE vpF vpG vpH) =
    showbCSC8 "Extend8" cH vpA vpB vpC vpD vpE vpF vpG vpH


-------------------------------- VariablePosition --------------------------------
-- Variable Position for abstract variable's real place
data VariablePosition
  = VP VariablePlace VariableIndex
  deriving (Eq, Ord)

instance Show VariablePosition where
  show = toString . showb

instance TextShow VariablePosition where
  showb (VP vPlace vIndex) =
    showb vPlace <> TS.singleton '[' <> showb vIndex <> TS.singleton ']'

-- TODO: Need to implement Eq/Ord instances for AtWorld and AtTime based on worldTime
-- instance Eq VariablePosition
-- instance Ord VariablePosition


-------------------------------- VariablePlace --------------------------------

data VariablePlace
  = AtTricky | AtPtr
  | AtWorld | AtTime | AtNWorld | AtNTime
  | AtDict | AtNDict | AtVars | AtNVars
  | AtLVars | AtLNVars | AtLTemp | AtLNTemp
  | AtReg | AtHere | AtNull
  deriving (Eq, Ord, Enum, Bounded, Read)

instance Show VariablePlace where
  show = toString . showb

instance TextShow VariablePlace where
  showb AtTricky = fromText "AtTricky"
  showb AtPtr    = fromText "AtPtr"
  showb AtWorld  = fromText "AtWorld"
  showb AtTime   = fromText "AtTime"
  showb AtNWorld = fromText "AtNWorld"
  showb AtNTime  = fromText "AtNTime"
  showb AtDict   = fromText "AtDict"
  showb AtNDict  = fromText "AtNDict"
  showb AtVars   = fromText "AtVars"
  showb AtNVars  = fromText "AtNVars"
  showb AtLVars  = fromText "AtLVars"
  showb AtLNVars = fromText "AtLNVars"
  showb AtLTemp  = fromText "AtLTemp"
  showb AtLNTemp = fromText "AtLNTemp"
  showb AtReg    = fromText "AtReg"
  showb AtHere   = fromText "AtHere"
  showb AtNull   = fromText "AtNull"


-------------------------------- VariableIndex --------------------------------

-- TODO: Arrange & reorder for matching with VI* and PVI*
data VariableIndex
  = VII Idx | VIN NKey
  | VIpN NKey | VIIT Idx Time | VINT NKey Time | VIpNT NKey Time
  | VIIRI Idx [Idx] | VINRI NKey [Idx] | VIpNRI NKey [Idx]
  | VIIRIT Idx [Idx] Time | VINRIT NKey [Idx] Time | VIpNRIT NKey [Idx] Time
  | VIV Value | VIAtom | VINull | VIPtr VariablePosition
  | PVII Idx | PVIN NKey | PVIpN NKey | PVIT Time
  | PVIIRI Idx [Idx] | PVINRI NKey [Idx] | PVIpNRI NKey [Idx]
  | PVIIRIT Idx [Idx] Time | PVINRIT NKey [Idx] Time | PVIpNRIT NKey [Idx] Time
  deriving (Eq, Ord)

instance Show VariableIndex where
  show = toString . showb

instance TextShow VariableIndex where
  showb (VII  idx                 )  = showb1 "VII" idx
  showb (VIN  nKey                )  = showb1 "VIN" nKey
  showb (VIpN nKey                )  = showb1 "VIpN" nKey
  showb (VIIT   idx  time         )  = showb2 "VIIT" idx time
  showb (VINT   nKey time         )  = showb2 "VINT" nKey time
  showb (VIpNT  nKey time         )  = showb2 "VIpNT" nKey time
  showb (VIIRI  idx  indices      )  = showb2 "VIIRI" idx indices
  showb (VINRI  nKey indices      )  = showb2 "VINRI" nKey indices
  showb (VIpNRI nKey indices      )  = showb2 "VIpNRI" nKey indices
  showb (VIIRIT  idx  indices time)  = showb3 "VIIRIT" idx indices time
  showb (VINRIT  nKey indices time)  = showb3 "VINRIT" nKey indices time
  showb (VIpNRIT nKey indices time)  = showb3 "VIpNRIT" nKey indices time
  showb (VIV value                )  = showb1 "VIV" value
  showb VIAtom                       = fromText "VIAtom"
  showb VINull                       = fromText "VINull"
  showb (VIPtr vp                  ) = showb1 "VIPtr" vp
  showb (PVII  idx                 ) = showb1 "PVII" idx
  showb (PVIN  nKey                ) = showb1 "PVIN" nKey
  showb (PVIpN nKey                ) = showb1 "PVIpN" nKey
  showb (PVIT  time                ) = showb1 "PVIT" time
  showb (PVIIRI  idx  indices      ) = showb2 "PVIIRI" idx indices
  showb (PVINRI  nKey indices      ) = showb2 "PVINRI" nKey indices
  showb (PVIpNRI nKey indices      ) = showb2 "PVIpNRI" nKey indices
  showb (PVIIRIT  idx  indices time) = showb3 "PVIIRIT" idx indices time
  showb (PVINRIT  nKey indices time) = showb3 "PVINRIT" nKey indices time
  showb (PVIpNRIT nKey indices time) = showb3 "PVIpNRIT" nKey indices time


-------------------------------- Value --------------------------------

-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
data Value
  = IntValue { iV :: Int }
  | DblValue { dV :: Double }
  | StrValue { sV :: Str }
  | BoolValue { bV :: Bool }
  | AtomValue
  | ArrValue { aV :: Array Value}
  | PtrValue { pV :: VariablePosition }
  | ScrValue { cV :: CEREScript }
  | ErrValue { errMessage :: Message }
  deriving (Eq, Ord)

instance Show Value where
  show = toString . showb

instance TextShow Value where
  showb (IntValue  i) = fromText "IV<| " <> showb i <> fromText " |>"
  showb (DblValue  d) = fromText "DV<| " <> showb d <> fromText " |>"
  showb (StrValue  s) = fromText "SV<| " <> fromText s <> fromText " |>"
  showb (BoolValue b) = fromText "BV<| " <> showb b <> fromText " |>"
  showb AtomValue     = fromText "AV<| - |>"
  showb (PtrValue vp) = fromText "PV<| " <> showb vp <> " |>"
  showb (ScrValue c ) = fromText "CV<| " <> showb c <> " |>"
  showb (ArrValue a ) = fromText "A[" <> showbArray a <> "]"
   where
    showbArray :: Array Value -> Builder
    showbArray a = if IM.null a
      then fromText "||  ||"
      else IM.foldrWithKey
        (\i v -> (<> TS.singleton ' ' <> showbElem i v <> fromText " ||"))
        (fromText "||")
        a
    showbElem :: Idx -> Value -> Builder
    showbElem i v = showb i <> TS.singleton ':' <> showb v
  showb (ErrValue e) = fromText "EV<| " <> fromText e <> fromText " |>"

showRaw :: Value -> String
showRaw = T.unpack . showRawT

showRawT :: Value -> Text
showRawT (IntValue  i) = showt i
showRawT (DblValue  d) = showt d
showRawT (StrValue  s) = s
showRawT (BoolValue b) = showt b
showRawT AtomValue     = "Atom"
showRawT (PtrValue vp) = showt vp
showRawT (ArrValue a ) = showt . IM.toList $ a
showRawT (ScrValue c ) = showt c
showRawT (ErrValue e ) = e


-------------------------------- ValueType --------------------------------

data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  | VTAtom
  | VTArr
  | VTPtr
  | VTScr
  | VTErr
  deriving (Eq, Ord, Enum, Read)

instance Show ValueType where
  show = toString . showb

instance TextShow ValueType where
  showb VTInt  = fromText "C-Int"
  showb VTDbl  = fromText "C-Dbl"
  showb VTStr  = fromText "C-Str"
  showb VTBool = fromText "CBool"
  showb VTAtom = fromText "CAtom"
  showb VTArr  = fromText "C-Arr"
  showb VTPtr  = fromText "C-Ptr"
  showb VTScr  = fromText "C-Scr"
  showb VTErr  = fromText "C-Err"


-------------------------------- ValueContainer --------------------------------

data ValueContainer = VC
  { value     :: Value
  , valueInfo :: ValueInfo
  } deriving (Show, Eq)

-- TODO: Not yet Implemented
data ValueInfo = ValueInfo
  { valueEdited       :: Bool
  , valueInheritance  :: ValueInheritanceFlag
  , valueDependencies :: [Branch]
  } deriving (Show, Eq)

data ValueInheritanceFlag = VIFInherit | VIFOnce deriving (Eq,Ord,Enum,Bounded)

instance Show ValueInheritanceFlag where
  show = toString . showb

instance TextShow ValueInheritanceFlag where
  showb VIFInherit = fromText "Inherit"
  showb VIFOnce    = fromText "Once"


-------------------------------- Helper for Value --------------------------------

data ValueTyper = ValueTyper
  { valueTyperName :: Name
  , valueType      :: ValueType
  } deriving (Show, Eq)
