module Data.CERES.Operator where


import qualified Data.Text.Lazy                as TL

import           TextShow


import           Data.CERES.Type


data CERESOperator
  = COAAdd
  | COAMul
  | COASub
  | COADiv
  | COAMod
  | COAEql
  | COACmp
  | COA Operator -- Custom Arithmetic Operator
  | CORSwp
  | CORMov
  | COR Operator -- Custom Register Operator
  | COTTake
  | COTDrop
  | COTSplit
  | COTTrim
  | COTAppend
  | COTConcat
  | COTInter
  | COTReplace
  | COTReverse
  | COTJustify
  | COTLength
  | COTIsNull
  | COTIsPrefix
  | COTIsInfix
  | COTIsSuffix
  | COT Operator -- Custom Register Operator
  deriving (Eq, Ord)

instance Show CERESOperator where
  show = toString . showb

instance TextShow CERESOperator where
  showb COAAdd     = fromLazyText "Add"
  showb COASub     = fromLazyText "Sub"
  showb COAMul     = fromLazyText "Mul"
  showb COADiv     = fromLazyText "Div"
  showb COAMod     = fromLazyText "Mod"
  showb COAEql     = fromLazyText "Eql"
  showb COACmp     = fromLazyText "Cmp"
  showb (COA o)    = fromText o
  showb CORSwp     = fromLazyText "Swp"
  showb CORMov     = fromLazyText "Mov"
  showb (COR o)    = fromText o
  showb COTTake    = fromLazyText "Take"
  showb COTDrop    = fromLazyText "Drop"
  showb COTSplit   = fromLazyText "Split"
  showb COTTrim    = fromLazyText "Trim"
  showb COTAppend  = fromLazyText "Append"
  showb COTConcat  = fromLazyText "Concat"
  showb COTInter   = fromLazyText "Inter"
  showb COTReplace = fromLazyText "Replace"
  showb COTReverse = fromLazyText "Reverse"
  showb COTJustify = fromLazyText "Justify"
  showb COTLength = fromLazyText "Length"
  showb COTIsNull = fromLazyText "IsNull"
  showb COTIsPrefix = fromLazyText "IsPrefix"
  showb COTIsInfix = fromLazyText "IsInfix"
  showb COTIsSuffix = fromLazyText "IsSuffix"
  showb (COT o)    = fromText o
