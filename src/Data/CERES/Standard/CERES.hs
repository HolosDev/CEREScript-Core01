module Data.CERES.Standard.CERES where


import qualified Data.Text                     as T

import           Data.CERES.Type
import           Data.CERES.Value


type VariablePlace = StandardVariablePlace
type VPosition = VariablePosition VariablePlace

data StandardVariablePlace
  = AtLocal
  | AtTime
  | AtWorld
  | AtDict
  | AtVar
  deriving (Eq, Ord, Enum,  Show)

type CEREScript = [CERES]

data CERES
  -- | Initialize Variable at VPosition A as Value B
  = InitVariable   VPosition Value
  -- TODO: InitVariableWith
  -- | Set Value at VPosition A as Value B
  | SetValue       VPosition Value
  -- | Delete Variable at VPosition A
  | DeleteVariable VPosition
  -- | Modify Value at VPosition A by CERESOperator with Value at VPosition B
  | ModifyValue    VPosition CERESOperator
  -- TODO: ModifyValueWith
  -- | Copy Value at VPosition B to Variable at VPosition A
  | CopyValue      VPosition VPosition
  -- | Convert type of Value at VPosition A as like as Value at VPosition B
  | ConvertValue   VPosition VPosition
  -- TODO: ConvertValueTo
  deriving (Eq,Ord,Show)

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
  deriving (Eq,Ord,Show)

data CERESSpool = CERESSpool
  { csID       :: ID
  , csName     :: Name
  , csScript   :: CEREScript
  , readVP     :: [VPosition]
  , writeVP    :: [VPosition]
  , csPriority :: Priority
  , csControl  :: [SpoolController]
  } deriving (Eq,Ord,Show)

data SpoolController
  = SCInherit ID
  | SCEnd     ID
  deriving (Eq,Ord,Show,Read)
