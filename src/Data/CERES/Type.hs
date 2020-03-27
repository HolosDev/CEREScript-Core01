module Data.CERES.Type where

import qualified Data.Text                     as T
                                                ( Text )
import           Data.Text.Lazy                 ( Text )

import           TextShow


type Name = Text
type NKey = Text
type Str = Text
type Time = Int
type ID = Int
type Idx = Int
type Message = Text

type Branch = T.Text
type Priority = Int


data Atom = Atom deriving (Eq,Ord,Enum,Read)

instance Show Atom where
  show Atom = "Atom"

instance TextShow Atom where
  showb Atom = fromLazyText "Atom"
