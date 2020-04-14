module Data.CERES.Data.Error where

import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow

import           Data.CERES.Type
import           Data.CERES.Data


errValueWith2 :: (TextShow a, TextShow b) => Name -> Message -> a -> b -> Value
errValueWith2 funcName errorType vA vB = ErrValue errorMessage
 where
  errorMessage =
    toLazyText
      $  fromLazyText "[Error]<"
      <> fromLazyText funcName
      <> fromLazyText " :=: "
      <> fromLazyText errorType
      <> fromLazyText "> "
      <> showb vA
      <> fromLazyText " and "
      <> showb vB

errValueTEWith2 :: (TextShow a, TextShow b) => Name -> a -> b -> Value
errValueTEWith2 funcName = errValueWith2 funcName "TypeError"
