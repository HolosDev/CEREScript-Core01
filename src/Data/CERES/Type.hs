module Data.CERES.Type where

import qualified Data.Text                     as T
                                                ( Text )
import           Data.Text.Lazy                 ( Text )


type Name    = Text
type Time    = Int
type ID      = Int
type Message = Text

type Branch   = T.Text
type Priority = Int
