module Data.CERES.Data.Method where


import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow


import           Data.CERES.Type
import           Data.CERES.Data


getInt :: Value -> Int
getInt (IntValue i) = i
getInt v            = error $ "[ERROR]<getInt> Given wrong type: " <> show v
getDbl :: Value -> Double
getDbl (DblValue d) = d
getDbl v            = error $ "[ERROR]<getDbl> Given wrong type: " <> show v
getStr :: Value -> Text
getStr (StrValue s) = s
getStr v            = error $ "[ERROR]<getStr> Given wrong type: " <> show v
getBool :: Value -> Bool
getBool (BoolValue b) = b
getBool v             = error $ "[ERROR]<getBool> Given wrong type: " <> show v
getAtom :: Value -> Atom
getAtom AtomValue = Atom
getAtom v         = error $ "[ERROR]<getAtom> Given wrong type: " <> show v
getPtr :: Value -> VariablePosition
getPtr (PtrValue vp) = vp
getPtr v             = error $ "[ERROR]<getPtr> Given wrong type: " <> show v
getArr :: Value -> Array Value
getArr (ArrValue a) = a
getArr v            = error $ "[ERROR]<getArr> Given wrong type: " <> show v
getScr :: Value -> CEREScript
getScr (ScrValue c) = c
getScr v            = error $ "[ERROR]<getScr> Given wrong type: " <> show v
getErr :: Value -> Message
getErr (ErrValue e) = e
getErr v            = error $ "[ERROR]<getErr> Given wrong type: " <> show v


getValueType :: Value -> ValueType
getValueType v = case v of
  (IntValue  _) -> VTInt
  (DblValue  _) -> VTDbl
  (StrValue  _) -> VTStr
  (BoolValue _) -> VTBool
  AtomValue     -> VTAtom
  (PtrValue _)  -> VTPtr
  (ArrValue _)  -> VTArr
  (ScrValue _)  -> VTScr
  (ErrValue _)  -> VTErr


-- FIXME: This is temporal instance when type CEREScript = [CERES]
showCEREScript :: CEREScript -> String
showCEREScript = toString . showbCEREScript
showtCEREScript :: CEREScript -> Text
showtCEREScript = toText . showbCEREScript
showtlCEREScript :: CEREScript -> TL.Text
showtlCEREScript = toLazyText . showbCEREScript
showbCEREScript :: CEREScript -> Builder
showbCEREScript = foldr1 (<>) . map (\v -> showb v <> singleton '\n')
