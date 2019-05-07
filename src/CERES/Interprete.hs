module CERES.Interprete where


import qualified Data.IntMap  as IM
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Maybe

import CERES.Operate
import Data.CERES
import Data.CERES.Type
import Data.CERES.Value


type Env = IM.IntMap Value
type CEREScript = [CERES]

runCERES :: CEREScript -> Env -> Env -> IO (Env,Env)
runCERES [] env localEnv = return (env, localEnv)
runCERES (aCERES:ceresList) env localEnv = do
  putStrLn "----------------------------------------------------------------"
  let (newEnv, newLocalEnv) = interpreteCERES aCERES env localEnv
  viewEnv "GEnv" newEnv
  putStrLn "-----------------"
  viewEnv "LEnv" newLocalEnv
  runCERES ceresList newEnv newLocalEnv

viewEnv :: Name -> Env -> IO ()
viewEnv envName =
  mapM_ (\x -> T.putStrLn . T.concat  $ [ envName, T.pack . show $ x ])

interpreteCERES (InitValue vc value) env localEnv = (newEnv,newLocalEnv)
  where
    (newEnv,newLocalEnv) = case variablePlace vc of
      AtLocal -> (env, IM.insert (variableID vc) value localEnv)
      AtWorld -> (IM.insert (variableID vc) value env, localEnv)

interpreteCERES (SetValue vc value) env localEnv = (newEnv,newLocalEnv)
  where
    (newEnv,newLocalEnv) = case variablePlace vc of
      AtLocal -> (env, IM.insert (variableID vc) value localEnv)
      AtWorld -> (IM.insert (variableID vc) value env, localEnv)

interpreteCERES (DeleteValue vc) env localEnv = (newEnv,newLocalEnv)
  where
    (newEnv,newLocalEnv) = case variablePlace vc of
      AtLocal -> (env, IM.delete (variableID vc) localEnv)
      AtWorld -> (IM.delete (variableID vc) env, localEnv)

interpreteCERES (ModifyValue vc operator) env localEnv = (newEnv,newLocalEnv)
  where
    mValue = case variablePlace vc of
      AtLocal -> IM.lookup (variableID vc) localEnv
      AtWorld -> IM.lookup (variableID vc) env
    newValue
      = maybe
          (errValueWith2
            "interpreteCERES-ModifyValue"
            "NotFound"
            vc
            operator)
          (modifyValue operator localEnv) mValue
    (newEnv,newLocalEnv) = case variablePlace vc of
      AtLocal -> (env, IM.insert (variableID vc) newValue localEnv)
      AtWorld -> (IM.insert (variableID vc) newValue env, localEnv)
