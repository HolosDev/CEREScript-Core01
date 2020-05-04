module Data.CERES.Data.Error where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           TextShow

import           Data.CERES.Type
import           Data.CERES.Data


errValueWith2 :: (TextShow a, TextShow b) => Name -> Message -> a -> b -> Value
errValueWith2 funcName errorType vA vB = ErrValue errorMessage
 where
  errorMessage =
    toText
      $  fromText "[Error]<"
      <> fromText funcName
      <> fromText " :=: "
      <> fromText errorType
      <> fromText "> "
      <> showb vA
      <> fromText " and "
      <> showb vB

errValueTEWith2 :: (TextShow a, TextShow b) => Name -> a -> b -> Value
errValueTEWith2 funcName = errValueWith2 funcName "TypeError"
