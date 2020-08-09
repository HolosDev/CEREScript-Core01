module Data.CERES.Type where

import Data.IntMap (IntMap)
import Data.Text (Text)
import TextShow


type Name = Text
type NKey = Text
type Operator = Text
type Category = Text
type Header = Text
type IHeader = Header
type CHeader = Header

type Str = Text
type Time = Int
type ID = Int
type Idx = Int
type Message = Text

type Array a = IntMap a

type Branch = Text
type Priority = Int


data Atom = Atom deriving (Eq,Ord,Enum,Read)

instance Show Atom where
  show Atom = "Atom"

instance TextShow Atom where
  showb Atom = fromLazyText "Atom"
