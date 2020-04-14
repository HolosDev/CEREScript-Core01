module CERES.Operate where


import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Read           as TL

import           TextShow

import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Data
import           Data.CERES.Data.Error


-- TODO: Retry with https://phab.quietjoon.net/T178
coaAdd, coaSub, coaMul, coaDiv, coaMod :: Value -> Value -> Value
coaAdd (IntValue rVA) (IntValue rVB) = IntValue $ rVA + rVB
coaAdd (DblValue rVA) (DblValue rVB) = DblValue $ rVA + rVB
coaAdd vA             vB             = errValueTEWith2 "coaAdd" vA vB
coaSub (IntValue rVA) (IntValue rVB) = IntValue $ rVA - rVB
coaSub (DblValue rVA) (DblValue rVB) = DblValue $ rVA - rVB
coaSub vA             vB             = errValueTEWith2 "coaSub" vA vB
coaMul (IntValue rVA) (IntValue rVB) = IntValue $ rVA * rVB
coaMul (DblValue rVA) (DblValue rVB) = DblValue $ rVA * rVB
coaMul vA             vB             = errValueTEWith2 "coaMul" vA vB
coaDiv (IntValue rVA) (IntValue rVB) = IntValue $ div rVA rVB
coaDiv (DblValue rVA) (DblValue rVB) = DblValue $ rVA / rVB
coaDiv vA             vB             = errValueTEWith2 "coaDiv" vA vB
coaMod (IntValue rVA) (IntValue rVB) = IntValue $ mod rVA rVB
coaMod vA             vB             = errValueTEWith2 "coaMod" vA vB

convertValue :: Value -> Value -> Value
convertValue vA             AtomValue    = AtomValue
convertValue vA             (StrValue _) = StrValue . showRawTL $ vA
convertValue (StrValue rVA) (IntValue _) = read
 where
  eRead = TL.decimal rVA
  read  = case eRead of
    (Right (rV, _)) -> IntValue rV
    (Left  _      ) -> errValueWith2 "convertValue" "Str -> Int" rVA VTInt
convertValue (StrValue rVA) (DblValue _) = read
 where
  eRead = TL.rational rVA
  read  = case eRead of
    (Right (rV, _)) -> DblValue rV
    (Left  _      ) -> errValueWith2 "convertValue" "Str -> Dbl" rVA VTDbl
convertValue (StrValue rVA) (BoolValue _) = case rVA of
  "True"  -> BoolValue True
  "False" -> BoolValue False
  "true"  -> BoolValue True
  "false" -> BoolValue False
  "T"     -> BoolValue True
  "F"     -> BoolValue False
  "TRUE"  -> BoolValue True
  "FALSE" -> BoolValue False
  "1"     -> BoolValue True
  "0"     -> BoolValue False
  _       -> errValueWith2 "convertValue" "Str -> Bool" rVA VTBool
convertValue (   StrValue rVA) (ErrValue _) = ErrValue rVA
convertValue vA@(IntValue _  ) (IntValue _) = vA
convertValue vA (IntValue _) =
  errValueWith2 "convertValue" "Not-Num -> Int" vA VTInt
convertValue (   IntValue rVA) (DblValue _) = DblValue . fromIntegral $ rVA
convertValue vA@(DblValue _  ) (DblValue _) = vA
convertValue vA (DblValue _) =
  errValueWith2 "convertValue" "Not-Num -> Dbl" vA VTInt
convertValue vA@(BoolValue _) (BoolValue _) = vA
convertValue vA (BoolValue _) =
  errValueWith2 "convertValue" "Not-Num -> Int" vA VTInt
convertValue vA@(ErrValue _) (ErrValue _) = vA
convertValue vA              (ErrValue _) = ErrValue . showRawTL $ vA
convertValue vA              vB           = errValueTEWith2 "convertValue" vA vB

operatorSelector :: CERESOperator -> (Value -> Value -> Value)
operatorSelector operator = case operator of
  COAAdd -> coaAdd
  COASub -> coaSub
  COAMul -> coaMul
  COADiv -> coaDiv
  COAMod -> coaMod
  _      -> error "No such operator"
