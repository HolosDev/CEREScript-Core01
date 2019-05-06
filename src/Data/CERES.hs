module Data.CERES where


import qualified Data.Text as T

import Data.CERES.Type
import Data.CERES.Value


type ValuePlace = StandardValuePlace
type VContainer = ValueContainer ValuePlace

data CERES
  -- | Initialize Value at VContainer A as Value B
  = InitValue    VContainer Value
  -- | Set Value at VContainer A as Value B
  -- TODO: InitValueWith
  | SetValue     VContainer Value
  -- | Delete Value at VContainer A
  | DeleteValue  VContainer
  -- | Modify Value at VContainer A by CERESOperator with VContainer B
  | ModifyValue  VContainer CERESOperator
  -- TODO: ModifyValueWith
  -- | Copy Value at VContainer A to Value at VContainer B
  | CopyValue    VContainer VContainer
  -- | Convert type of Value at VContainer A as like as VContainer B
  | ConvertValue VContainer VContainer
  -- TODO: ConvertValueTo

data CERESOperator
  = COAMul
  | COAAdd
  | COASub
  | COADiv
  | COAMod
  | COAMulWith Value
  | COAAddWith Value
  | COASubWith Value
  | COADivWith Value
  | COAModWith Value
  deriving Show
