module CERES.Interprete where


import qualified Data.Map     as M
import qualified Data.IntMap  as IM
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Maybe

import CERES.Operate
import Data.CERES
import Data.CERES.Type
import Data.CERES.Value


-- TODO: How to distinguish
--  * 1. Refered, Added, or Changed
--  * 2. Refered, but deleted

type EnvSet = (Env,Env)
-- TODO: Value -> ValueContainer
type Env = IM.IntMap Value
type VV = (VPosition, Maybe Value)
type VVMap = M.Map VPosition (Maybe Value)
type CEREScript = [CERES]

runCERES :: CEREScript -> EnvSet -> IO EnvSet
runCERES [] (env, localEnv) = return (env, localEnv)
runCERES (aCERES:ceresList) (env, localEnv) = do
  putStrLn "----------------------------------------------------------------"
  (newEnv, newLocalEnv) <- interpreteCERES aCERES (env, localEnv)
  viewEnv "GEnv" newEnv
  putStrLn "-----------------"
  viewEnv "LEnv" newLocalEnv
  runCERES ceresList (newEnv, newLocalEnv)

viewEnv :: Name -> Env -> IO ()
viewEnv envName =
  mapM_ (\x -> T.putStrLn . T.concat  $ [ envName, T.pack . show $ x ])

interpreteCERES :: CERES -> EnvSet -> IO EnvSet
interpreteCERES anInstruction envSet = do
  let rvMap = readByInstruction envSet anInstruction M.empty
  vvMap <- runOperator anInstruction rvMap
  return $ setValuesToEnvSet envSet (M.toList vvMap)

-- TODO: Should be recursively
readByInstruction :: EnvSet -> CERES -> VVMap -> VVMap
readByInstruction envSet anInstruction rvMap =
  case anInstruction of
    (InitVariable _ _) -> rvMap
    (SetValue _ _)  -> rvMap
    (DeleteVariable _) -> rvMap
    -- TODO: Shoud be more complicate form
    (ModifyValue vp _) -> readValueFromEnvSet envSet rvMap [vp,(VP 0 AtLocal)]

readValueFromEnvSet :: EnvSet -> VVMap -> [VPosition] -> VVMap
readValueFromEnvSet _ vvMap [] = vvMap
readValueFromEnvSet envSet@(env, localEnv) vvMap (vp:rest) =
  readValueFromEnvSet envSet (M.insert vp (Just theValue) vvMap) rest
  where
    mValue = case variablePlace vp of
      AtLocal -> IM.lookup (variableID vp) localEnv
      AtWorld -> IM.lookup (variableID vp) env
    theValue =
      fromMaybe (ErrValue $
        T.concat
          [ "readValueFromEnvSet - NotFound at "
          , T.pack (show vp)
          ])
        mValue

setValuesToEnvSet :: EnvSet -> [VV] -> EnvSet
setValuesToEnvSet envSet [] = envSet
setValuesToEnvSet (env, localEnv) ((vp,value):rest) = setValuesToEnvSet (newEnv,newLocalEnv) rest
  where
    (newEnv,newLocalEnv) = case variablePlace vp of
      AtLocal -> (env, alterOrDelete (variableID vp) value localEnv)
      AtWorld -> (alterOrDelete (variableID vp) value env, localEnv)

alterOrDelete :: ID -> Maybe Value -> Env -> Env
alterOrDelete mID Nothing env = IM.delete mID env
alterOrDelete mID (Just value) env = IM.insert mID value env

runOperator :: CERES -> VVMap -> IO VVMap
runOperator anInstruction vvMap = return $
  case anInstruction of
    (InitVariable vp value) -> M.insert vp (Just value) vvMap
    (SetValue vp value) -> M.insert vp (Just value) vvMap
    (DeleteVariable vp) -> M.insert vp Nothing vvMap
    (ModifyValue vp operator) -> M.insert vp (Just newValue) vvMap
      where
        newValue :: Value
        newValue = case operator of
          COAMul -> caoMul
            (fromMaybe (errValueWith2 "runOperator" "RefersDeletedVariable" vp operator)
              (fromMaybe (Just (errValueWith2 "runOperator" "NotFound" vp operator)) (vvMap M.!? vp)))
            (fromMaybe (errValueWith2 "runOperator" "RefersDeletedVariable" vp operator)
              (fromMaybe (Just (errValueWith2 "runOperator" "NotFound" vp operator)) (vvMap M.!? (VP 0 AtLocal))))
          _ -> error $ "[ERROR]<runOperator>: Not yet implemented operator: " ++ show operator
