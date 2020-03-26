module Data.CERES.VariablePosition where

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL

import           TextShow

import           Data.CERES.Type
import           Data.CERES.Value


-- Variable Position for abstract variable's real place
data VariablePosition
  = AtWorld Time ID
  | AtTime Time ID
  | AtDict ID
  | AtNDict Name
  | AtVar ID
  | AtLocal ID
  | AtCache ID
  | AtHere Value
  | AtNull
  deriving (Eq, Ord, Read)

instance Show VariablePosition where
  show = TL.unpack . showtl

instance TextShow VariablePosition where
  showb (AtWorld tIdx idx) =
    fromLazyText "AtWorld[" <> showb tIdx <> fromLazyText ":" <> showb idx <> fromLazyText "]"
  showb (AtTime tIdx idx) =
    fromLazyText "AtTime[" <> showb tIdx <> fromLazyText ":" <> showb idx <> fromLazyText "]"
  showb (AtDict idx) = fromLazyText "AtDict[" <> showb idx <> fromLazyText "]"
  showb (AtNDict name) = fromLazyText "AtNDict[" <> fromLazyText name <> fromLazyText "]"
  showb (AtVar idx)  = fromLazyText "AtVar[" <> showb idx <> fromLazyText "]"
  showb (AtLocal idx) = fromLazyText "AtLocal[" <> showb idx <> fromLazyText "]"
  showb (AtCache idx) = fromLazyText "AtCache[" <> showb idx <> fromLazyText "]"
  showb (AtHere v)  = fromLazyText "AtHere[" <> showb v <> fromLazyText "]"
  showb AtNull  = fromLazyText "AtNull"

-- TODO: Need to implement Eq/Ord for AtWorld and AtTime based on worldTime
-- instance Eq VariablePosition
-- instance Ord VariablePosition
