module Test where


import Data.CallStack

import Test.HUnit.Base
import Test.HUnit.Lang


assertNotEqual label _a _b = assertBool label (_a /= _b)

(@?/=) :: (HasCallStack, Eq a, Show a)
                        => a -- ^ The actual value
                        -> a -- ^ The expected value
                        -> Assertion
actual @?/= expected = assertNotEqual "" expected actual

(@/=?) :: (HasCallStack, Eq a, Show a)
                        => a -- ^ The expected value
                        -> a -- ^ The actual value
                        -> Assertion
expected @/=? actual = assertNotEqual "" expected actual
