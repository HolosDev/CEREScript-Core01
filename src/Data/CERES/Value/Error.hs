module Data.CERES.Value.Error where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Data.CERES.Type
import           Data.CERES.Value


errValueWith2 :: (Show a, Show b) => Name -> Message -> a -> b -> Value
errValueWith2 funcName errorType vA vB = ErrValue errorMessage
 where
  errorMessage = T.concat
    [ "[Error]<"
    , funcName
    , " :=: "
    , errorType
    , "> "
    , T.pack . show $ vA
    , " and "
    , T.pack . show $ vB
    ]

errValueTEWith2 :: (Show a, Show b) => Name -> a -> b -> Value
errValueTEWith2 funcName = errValueWith2 funcName "TypeError"
