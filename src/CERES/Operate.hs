module CERES.Operate where


import qualified Data.IntMap.Lazy              as IM
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T

import           TextShow

import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Data
import           Data.CERES.Data.Error
import           Data.CERES.Data.Method


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

coaEql vA vB = BoolValue (vA == vB)

coaCmp (IntValue rVA) (IntValue rVB) = if rVA == rVB
  then IntValue 0
  else if rVA < rVB then IntValue 1 else IntValue (-1)
coaCmp (IntValue rVA) (DblValue rVB) = if fromIntegral rVA == rVB
  then IntValue 0
  else if fromIntegral rVA < rVB then IntValue 1 else IntValue (-1)
coaCmp (DblValue rVA) (IntValue rVB) = if rVA == fromIntegral rVB
  then IntValue 0
  else if rVA < fromIntegral rVB then IntValue 1 else IntValue (-1)
coaCmp (DblValue rVA) (DblValue rVB) = if rVA == rVB
  then IntValue 0
  else if rVA < rVB then IntValue 1 else IntValue (-1)
coaCmp vA vB = errValueTEWith2 "coaCmp" vA vB

cotTake (StrValue rVA) (IntValue rVB) = StrValue $ T.take rVB rVA
cotTake vA             vB             = errValueTEWith2 "cotTake" vA vB
cotDrop (StrValue rVA) (IntValue rVB) = StrValue $ T.drop rVB rVA
cotDrop vA             vB             = errValueTEWith2 "cotDrop" vA vB

cotAppend (StrValue rVA) (StrValue rVB) = StrValue $ rVA <> rVB
cotAppend vA             vB             = errValueTEWith2 "cotAppend" vA vB

cotInter (StrValue rVA) (ArrValue rVB) =
  error "[ERROR]<cotInter> Not yet implemented"
cotInter vA vB = errValueTEWith2 "cotInter" vA vB
cotReplace _ _ =
  error "[ERROR]<cotReplace> Should be implemented by Interpreter"
cotReplace vA vB = errValueTEWith2 "cotReplace" vA vB
cotJustify (StrValue rVA) (IntValue rVB) =
  error "[ERROR]<cotJustify> Should be implemented with 2-byte length handler"
cotJustify vA vB = errValueTEWith2 "cotJustify" vA vB
cotIsPrefix (StrValue rVA) (StrValue rVB) = BoolValue (T.isPrefixOf rVA rVB)
cotIsPrefix vA             vB             = errValueTEWith2 "cotIsPrefix" vA vB
cotIsInfix (StrValue rVA) (StrValue rVB) = BoolValue (T.isInfixOf rVA rVB)
cotIsInfix vA             vB             = errValueTEWith2 "cotIsInfix" vA vB
cotIsSuffix (StrValue rVA) (StrValue rVB) = BoolValue (T.isSuffixOf rVA rVB)
cotIsSuffix vA             vB             = errValueTEWith2 "cotIsSuffix" vA vB

coaNeg (IntValue rV) = IntValue (-rV)
coaNeg (DblValue rV) = DblValue (-rV)
coaNeg v             = errValueTEWith1 "coaNeg" v
cobNot (BoolValue rV) = BoolValue (not rV)
cobNot v              = errValueTEWith1 "cobNot" v
cotTrim (StrValue rV) = StrValue (T.strip rV)
cotTrim v             = errValueTEWith1 "cotTrim" v
cotConcat (ArrValue rV) = error "[ERROR]<cotConcat> Not yet implemented"
cotConcat v             = errValueTEWith1 "cotConcat" v
cotReverse (StrValue rV) = StrValue (T.reverse rV)
cotReverse v             = errValueTEWith1 "cotReverse" v
cotLength (StrValue rV) = IntValue (T.length rV)
cotLength (ArrValue rV) = IntValue (IM.size rV)
cotLength v             = errValueTEWith1 "cotLength" v
cotIsNull (StrValue rV) = BoolValue (T.null rV)
cotIsNull (ArrValue rV) = BoolValue (IM.null rV)
cotIsNull v             = errValueTEWith1 "cotIsNull" v


convertValue :: Value -> ValueType -> Value
convertValue vA             VTAtom = AtomValue
convertValue vA             VTStr  = StrValue . showRawT $ vA
convertValue (StrValue rVA) VTInt  = read
 where
  eRead = T.decimal rVA
  read  = case eRead of
    (Right (rV, _)) -> IntValue rV
    (Left  _      ) -> errValueWith2 "convertValue" "Str -> Int" rVA VTInt
convertValue (StrValue rVA) VTDbl = read
 where
  eRead = T.rational rVA
  read  = case eRead of
    (Right (rV, _)) -> DblValue rV
    (Left  _      ) -> errValueWith2 "convertValue" "Str -> Dbl" rVA VTDbl
convertValue (StrValue rVA) VTBool = case rVA of
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
convertValue (   StrValue rVA) VTErr  = ErrValue rVA
convertValue vA@(IntValue _  ) VTInt  = vA
convertValue vA VTInt = errValueWith2 "convertValue" "Not-Num -> Int" vA VTInt
convertValue (   IntValue rVA) VTDbl  = DblValue . fromIntegral $ rVA
convertValue vA@(DblValue _  ) VTDbl  = vA
convertValue vA VTDbl = errValueWith2 "convertValue" "Not-Num -> Dbl" vA VTInt
convertValue vA@(BoolValue _)  VTBool = vA
convertValue vA VTBool = errValueWith2 "convertValue" "Not-Num -> Int" vA VTInt
convertValue vA@(ErrValue _)   VTErr  = vA
convertValue vA                VTErr  = ErrValue . showRawT $ vA
convertValue vA                vB     = errValueTEWith2 "convertValue" vA vB

convertValueBy :: Value -> Value -> Value
convertValueBy vA vB = convertValue vA (getValueType vB)


operator2Selector :: CERESOperator -> Maybe (Value -> Value -> Value)
operator2Selector operator = case operator of
  COAAdd      -> Just coaAdd
  COASub      -> Just coaSub
  COAMul      -> Just coaMul
  COADiv      -> Just coaDiv
  COAMod      -> Just coaMod
  COAEql      -> Just coaEql
  COACmp      -> Just coaCmp
  COTTake     -> Just cotTake
  COTDrop     -> Just cotDrop
  COTAppend   -> Just cotAppend
  COTInter    -> Just cotInter
  COTReplace  -> Just cotReplace
  COTJustify  -> Just cotJustify
  COTIsPrefix -> Just cotIsPrefix
  COTIsInfix  -> Just cotIsInfix
  COTIsSuffix -> Just cotIsSuffix
  _           -> Nothing

{-
  COTSplit -> Just cotSplit
-}

operator1Selector :: CERESOperator -> Maybe (Value -> Value)
operator1Selector operator = case operator of
  COANeg     -> Just coaNeg
  COBNot     -> Just cobNot
  COTTrim    -> Just cotTrim
  COTConcat  -> Just cotConcat
  COTReverse -> Just cotReverse
  COTLength  -> Just cotLength
  COTIsNull  -> Just cotIsNull
  _          -> Nothing

{-
  CORSwp -> Just
  CORMov -> Just
-}
