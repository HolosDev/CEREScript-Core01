module Data.CERES.Value.Method where

import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           Data.CERES.Type
import           Data.CERES.Value


getInt :: Value -> Int
getInt (IntValue  i) = i
getInt v = error $ "[ERROR]<getInt> Given wrong type: " ++ show v
getDbl :: Value -> Double
getDbl (DblValue  d) = d
getDbl v = error $ "[ERROR]<getDbl> Given wrong type: " ++ show v
getStr :: Value -> Text
getStr (StrValue  s) = s
getStr v = error $ "[ERROR]<getStr> Given wrong type: " ++ show v
getBool :: Value -> Bool
getBool (BoolValue b) = b
getBool v = error $ "[ERROR]<getBool> Given wrong type: " ++ show v
getErr :: Value -> Message
getErr (ErrValue  e) = e
getErr v = error $ "[ERROR]<getErr> Given wrong type: " ++ show v
