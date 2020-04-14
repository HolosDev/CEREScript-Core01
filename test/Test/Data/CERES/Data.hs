{-# LANGUAGE TemplateHaskell #-}

module Test.Data.CERES.Data where


import           Test
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base

import           Data.CERES.Type
import           Data.CERES.Data


tests = $(testGroupGenerator)

i1 = IntValue 1
i2 = IntValue 2
s1 = StrValue "S"
s2 = StrValue "SA"
e1 = ErrValue "ABC"
bT = BoolValue True
bF = BoolValue False

c_eq01 = i1 @?/= i2
c_eq02 = i1 @?/= s1
c_eq03 = i1 @?= i1
c_eq04 = e1 @?= e1
c_eq05 = s1 @?= s1
c_eq06 = s1 @?/= s2
c_eq07 = bT @?/= bF
c_eq08 = bT @?= bT

test_ComparingValue =
  [ testCase "i1 /= i2" c_eq01
  , testCase "i1 /= s1" c_eq02
  , testCase "i1 == i1" c_eq03
  , testCase "e1 == e1" c_eq04
  , testCase "s1 == s1" c_eq05
  , testCase "s1 /= s2" c_eq06
  , testCase "bT /= bF" c_eq07
  , testCase "bT == bT" c_eq08
  ]
