module Data.CERES.Typical where

data StandardVariablePlace
  = AtLocal
  | AtTime
  | AtWorld
  | AtDict
  | AtVar
  deriving (Eq, Ord, Enum,  Show)
