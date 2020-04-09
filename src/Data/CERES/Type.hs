module Data.CERES.Type where

import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import           Data.Vector                    ( Vector )

import           TextShow
import           TextShow.Data.Vector


type Name = Text
type NKey = T.Text
type Str = Text
type Time = Int
type ID = Int
type Idx = Int
type Message = Text
type Array a = Vector a

type Branch = T.Text
type Priority = Int


data Atom = Atom deriving (Eq,Ord,Enum,Read)

instance Show Atom where
  show Atom = "Atom"

instance TextShow Atom where
  showb Atom = fromLazyText "Atom"
