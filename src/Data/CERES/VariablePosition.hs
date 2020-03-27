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
  = AtWrld Time Idx
  | AtTime Time Idx
  | AtDict Idx
  | AtNDic NKey
  | AtVars Idx
  | AtLocl Idx
  | AtCach Idx
  | AtHere Value
  | AtNull
  deriving (Eq, Ord, Read)

instance Show VariablePosition where
  show = TL.unpack . showtl

instance TextShow VariablePosition where
  showb (AtWrld tIdx idx) =
    fromLazyText "AtWrld["
      <> showb tIdx
      <> fromLazyText ":"
      <> showb idx
      <> fromLazyText "]"
  showb (AtTime tIdx idx) =
    fromLazyText "AtTime["
      <> showb tIdx
      <> fromLazyText ":"
      <> showb idx
      <> fromLazyText "]"
  showb (AtDict idx) = fromLazyText "AtDict[" <> showb idx <> fromLazyText "]"
  showb (AtNDic name) =
    fromLazyText "AtNDic[" <> fromLazyText name <> fromLazyText "]"
  showb (AtVars idx) = fromLazyText "AtVars[" <> showb idx <> fromLazyText "]"
  showb (AtLocl idx) = fromLazyText "AtLocl[" <> showb idx <> fromLazyText "]"
  showb (AtCach idx) = fromLazyText "AtCach[" <> showb idx <> fromLazyText "]"
  showb (AtHere v  ) = fromLazyText "AtHere[" <> showb v <> fromLazyText "]"
  showb AtNull       = fromLazyText "AtNull"

-- TODO: Need to implement Eq/Ord for AtWorld and AtTime based on worldTime
-- instance Eq VariablePosition
-- instance Ord VariablePosition
