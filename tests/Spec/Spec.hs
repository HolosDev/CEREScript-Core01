import qualified Test.Framework                as Test
import qualified Test.Framework.Providers.HUnit
                                               as Test
import qualified Test.Framework.Providers.QuickCheck2
                                               as Test
import           Test.HUnit
import           Test.QuickCheck

import           Test.Data.CERES.Data


main :: IO ()
main = do
  Test.defaultMain [Test.Data.CERES.Data.tests]
