module CERES.Standard.Operate where


import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T

import           Data.CERES.Standard.CERES
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.Value.Error


coaMul :: Value -> Value -> Value
coaMul (IntValue vA) (IntValue vB) = IntValue $ vA * vB
coaMul (DblValue vA) (DblValue vB) = DblValue $ vA * vB
coaMul vA            vB            = errValueTEWith2 "coaMul" vA vB
{-
coaAdd Value
coaSub Value
coaDiv Value
coaMod Value
-}

convertValue vA (StrValue _) = StrValue . showRawTL $ vA
convertValue (   IntValue rvA) (DblValue _) = DblValue . fromIntegral $ rvA
convertValue vA@(DblValue _  ) (DblValue _) = vA
convertValue vA (DblValue _) =
  errValueWith2 "convertValue" "Not-Int -> Dbl" vA VTDbl
-- TODO: Add * to Str function
convertValue vA vB = errValueTEWith2 "convertValue" vA vB

modifyValue operator localEnv value = newValue
 where
  mOperand = IM.lookup 0 localEnv
  newValue = maybe (ErrValue "Null register 0")
                   (operatorSelector operator value)
                   mOperand

operatorSelector operator = case operator of
  COAMul -> coaMul
  _      -> error "No such operator"

modifyValueStack []                     localEnv value = (localEnv, value)
-- TODO: Use `operator`, `localEnv`, `value`
modifyValueStack (operator : operators) localEnv value = modifyValueStack
  operators
  newLocalEnv
  newValue
 where
  newValue    = error ""
  newLocalEnv = error ""
