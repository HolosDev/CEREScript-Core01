module Data.CERES.Operator where


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
  | COANeg
  | COA Operator -- Custom Arithmetic Operator
  | COBNot
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
  | COT Operator -- Custom Text Operator
  | COE1 Operator -- Custom Extendable Operator
  | COE2 Operator -- Custom Extendable Operator
  | COE3 Operator -- Custom Extendable Operator
  | COE4 Operator -- Custom Extendable Operator
  deriving (Eq, Ord)

instance Show CERESOperator where
  show = toString . showb

instance TextShow CERESOperator where
  showb COAAdd      = fromText "<Add>"
  showb COASub      = fromText "<Sub>"
  showb COAMul      = fromText "<Mul>"
  showb COADiv      = fromText "<Div>"
  showb COAMod      = fromText "<Mod>"
  showb COAEql      = fromText "<Eql>"
  showb COACmp      = fromText "<Cmp>"
  showb COANeg      = fromText "<Neg>"
  showb (COA o)     = fromText "<A:" <> fromText o <> singleton '>'
  showb COBNot      = fromText "<Not>"
  showb CORSwp      = fromText "<Swp>"
  showb CORMov      = fromText "<Mov>"
  showb (COR o)     = fromText "<R:" <> fromText o <> singleton '>'
  showb COTTake     = fromText "<Take>"
  showb COTDrop     = fromText "<Drop>"
  showb COTSplit    = fromText "<Split>"
  showb COTTrim     = fromText "<Trim>"
  showb COTAppend   = fromText "<Append>"
  showb COTConcat   = fromText "<Concat>"
  showb COTInter    = fromText "<Inter>"
  showb COTReplace  = fromText "<Replace>"
  showb COTReverse  = fromText "<Reverse>"
  showb COTJustify  = fromText "<Justify>"
  showb COTLength   = fromText "<Length>"
  showb COTIsNull   = fromText "<IsNull>"
  showb COTIsPrefix = fromText "<IsPrefix>"
  showb COTIsInfix  = fromText "<IsInfix>"
  showb COTIsSuffix = fromText "<IsSuffix>"
  showb (COT  o)    = fromText "<T:" <> fromText o <> singleton '>'
  showb (COE1 o)    = fromText "<E1:" <> fromText o <> singleton '>'
  showb (COE2 o)    = fromText "<E2:" <> fromText o <> singleton '>'
  showb (COE3 o)    = fromText "<E3:" <> fromText o <> singleton '>'
  showb (COE4 o)    = fromText "<E4:" <> fromText o <> singleton '>'
