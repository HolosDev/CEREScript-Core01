module Data.CERES.Value where

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
import           Data.Trie.Text                 ( Trie )
import           Data.Trie.Text                as Trie

import           TextShow                      as TS

import           Data.CERES.Type


type ValueMap = IntMap Value
type ValueNMap = Trie Value

blankVM = IM.empty
blankVNM = Trie.empty

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
  | StrValue { sV :: Str }
  | BoolValue { bV :: Bool }
  | AtomValue
  | ArrValue { aV :: Array Value}
  | ErrValue { errMessage :: Message }
  deriving (Eq, Ord)

instance Show Value where
  show = TL.unpack . showtl

showRaw :: Value -> String
showRaw (IntValue  i) = show i
showRaw (DblValue  d) = show d
showRaw (StrValue  s) = TL.unpack s
showRaw (BoolValue b) = show b
showRaw AtomValue     = "Atom"
showRaw (ArrValue a)  = show a
showRaw (ErrValue e)  = TL.unpack e

instance TextShow Value where
  showb (IntValue i) = fromLazyText "IV<| " <> showb i <> fromLazyText " |>"
  showb (DblValue d) = fromLazyText "DV<| " <> showb d <> fromLazyText " |>"
  showb (StrValue s) =
    fromLazyText "SV<| " <> fromLazyText s <> fromLazyText " |>"
  showb (BoolValue b) = fromLazyText "BV<| " <> showb b <> fromLazyText " |>"
  showb AtomValue     = fromLazyText "AV<| - |>"
  showb (ArrValue a)  = fromLazyText "A[" <> showbArray a <> "]"
   where
    showbArray :: Array Value -> Builder
    showbArray a =
      if IM.null a
        then fromLazyText "||  ||"
        else IM.foldrWithKey (\i v -> (<> TS.singleton ' ' <> showbElem i v <> TS.fromLazyText " ||")) (TS.fromLazyText "||") a
    showbElem :: Idx -> Value -> Builder
    showbElem i v = showb i <> TS.singleton ':' <> showb v
  showb (ErrValue e) =
    fromLazyText "EV<| " <> fromLazyText e <> fromLazyText " |>"

showRawTL :: Value -> Text
showRawTL (IntValue  i) = showtl i
showRawTL (DblValue  d) = showtl d
showRawTL (StrValue  s) = s
showRawTL (BoolValue b) = showtl b
showRawTL AtomValue     = "Atom"
showRawTL (ArrValue a)  = showtl . IM.toList $ a
showRawTL (ErrValue e)  = e


data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  | VTAtom
  | VTArr
  | VTErr
  deriving (Eq, Ord, Enum, Read)

instance Show ValueType where
  show VTInt  = "C-Int"
  show VTDbl  = "C-Dbl"
  show VTStr  = "C-Str"
  show VTBool = "CBool"
  show VTAtom = "CAtom"
  show VTArr  = "CArr"
  show VTErr  = "C-Err"

instance TextShow ValueType where
  showb VTInt  = fromLazyText "C-Int"
  showb VTDbl  = fromLazyText "C-Dbl"
  showb VTStr  = fromLazyText "C-Str"
  showb VTBool = fromLazyText "CBool"
  showb VTAtom = fromLazyText "CAtom"
  showb VTArr  = fromLazyText "C-Arr"
  showb VTErr  = fromLazyText "C-Err"
