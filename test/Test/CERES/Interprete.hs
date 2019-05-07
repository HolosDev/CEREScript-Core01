{-# LANGUAGE TemplateHaskell #-}

module Test.CERES.Interprete where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit.Base

import qualified Data.IntMap as IM

import CERES.Interprete
import CERES.Operate
import Data.CERES
import Data.CERES.Type
import Data.CERES.Value

tests = $(testGroupGenerator)

i1 = IntValue 1
i2 = IntValue 2
d1 = DblValue 1
s1 = StrValue "S"

emptyEnv = IM.empty
initialEnv = IM.fromList [(1, i1), (3, s1)]
env_set01 = IM.fromList [(1, i2), (3, s1)]
env_set02 = IM.fromList [(1, d1), (3, s1)]
env_set03 = IM.fromList [(1, s1), (3, s1)]

env_modify01 = IM.fromList [(1, i1), (2, errValueWith2 "interpreteCERES-ModifyValue" "NotFound" (VP 2 AtWorld) (COAMulWith (IntValue 4))), (3, s1)]

c_set01 = (env_set01,emptyEnv) @?= interpreteCERES (SetValue (VP 1 AtWorld) i2) initialEnv emptyEnv
c_set02 = (env_set02,emptyEnv) @?= interpreteCERES (SetValue (VP 1 AtWorld) d1) initialEnv emptyEnv
c_set03 = (env_set03,emptyEnv) @?= interpreteCERES (SetValue (VP 1 AtWorld) s1) initialEnv emptyEnv

c_modify01 = (env_modify01,emptyEnv) @?= interpreteCERES (ModifyValue (VP 2 AtWorld) (COAMulWith (IntValue 4))) initialEnv emptyEnv

test_BasicInterprete =
  [ testCase ("SetValue 1 " ++ show i2) c_set01
  , testCase ("SetValue 1 " ++ show d1) c_set02
  , testCase ("SetValue 1 " ++ show s1) c_set03
  , testCase "ModifyValue 2" c_modify01
  ]
