{-# LANGUAGE TemplateHaskell #-}

module Test.CERES.Interpret where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit.Base

import qualified Data.IntMap as IM
import qualified Data.Text as T

import CERES.Standard.Interpret
import CERES.Standard.Operate
import Data.CERES.Standard.CERES
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

env_modify01 = IM.fromList
  [ (1, i1)
  , ( 2
    , errValueTEWith2
      "coaMul"
      (ErrValue $ T.concat
        [ "readValueFromEnvSet - NotFound at "
        , T.pack (show (VP 2 AtWorld voidHere))
        ]
      )
      (IntValue 4)
    )
  , (3, s1)
  ]

c_set01 = do
  result <- interpretCERES (SetValue (VP 1 AtWorld voidHere) i2) (initialEnv, emptyEnv)
  result @?= (env_set01,emptyEnv)
c_set02 = do
  result <- interpretCERES (SetValue (VP 1 AtWorld voidHere) d1) (initialEnv, emptyEnv)
  result @?= (env_set02,emptyEnv)
c_set03 = do
  result <- interpretCERES (SetValue (VP 1 AtWorld voidHere) s1) (initialEnv, emptyEnv)
  result @?= (env_set03,emptyEnv)

c_modify01 = do
  result <- interpretCERES (ModifyValue (VP 2 AtWorld voidHere) (COAMulWith (IntValue 4))) (initialEnv, emptyEnv)
  result @?= (env_modify01,emptyEnv)

test_BasicInterpret =
  [ testCase ("SetValue 1 " ++ show i2) c_set01
  , testCase ("SetValue 1 " ++ show d1) c_set02
  , testCase ("SetValue 1 " ++ show s1) c_set03
  , testCase "ModifyValue 2" c_modify01
  ]
