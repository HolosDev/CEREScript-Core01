module CERES.Interpret where


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
--  * 1. Referred, Added, or Changed
--  * 2. Referred, but deleted

-- TODO: Value -> ValueContainer
type Env = IM.IntMap Value
viewEnv :: Name -> Env -> IO ()
viewEnv envName =
  mapM_ (\x -> T.putStrLn . T.concat  $ [ envName, T.pack . show $ x ])

type EnvSet = (Env,Env)
type VV = (VPosition, RW (Maybe Value))
type VVMap = M.Map VPosition (RW (Maybe Value))

data RW a = R a | W a | RW a
runRW (R a) = a
runRW (W a) = a
runRW (RW a) = a
notR (R _) = False
notR _ = True

runCERES :: CEREScript -> EnvSet -> IO EnvSet
runCERES [] (env, localEnv) = return (env, localEnv)
runCERES (aCERES:ceresList) (env, localEnv) = do
  putStrLn "----------------------------------------------------------------"
  (newEnv, newLocalEnv) <- interpretCERES aCERES (env, localEnv)
  viewEnv "GEnv" newEnv
  putStrLn "-----------------"
  viewEnv "LEnv" newLocalEnv
  runCERES ceresList (newEnv, newLocalEnv)

interpretCERES :: CERES -> EnvSet -> IO EnvSet
interpretCERES anInstruction envSet = do
  let rvMap = readByInstruction envSet anInstruction M.empty
  vvMap <- runOperator anInstruction rvMap
  return $ setValuesToEnvSet envSet (filter (notR . snd) . M.toList $ vvMap)

-- TODO: Should be recursively
readByInstruction :: EnvSet -> CERES -> VVMap -> VVMap
readByInstruction envSet anInstruction rvMap =
  case anInstruction of
    (InitVariable _ _) -> rvMap
    (SetValue _ _)  -> rvMap
    (DeleteVariable _) -> rvMap
    -- TODO: Should be more complicate form
    (ModifyValue vp _) -> readValueFromEnvSet envSet rvMap [vp,(VP 0 AtLocal)]

readValueFromEnvSet :: EnvSet -> VVMap -> [VPosition] -> VVMap
readValueFromEnvSet _ vvMap [] = vvMap
readValueFromEnvSet envSet@(env, localEnv) vvMap (vp:rest) =
  readValueFromEnvSet envSet (M.insert vp (R (Just theValue)) vvMap) rest
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
setValuesToEnvSet (env, localEnv) ((vp,rwMValue):rest) = setValuesToEnvSet (newEnv,newLocalEnv) rest
  where
    (newEnv,newLocalEnv) = case variablePlace vp of
      AtLocal -> (env, alterOrDelete (variableID vp) rwMValue localEnv)
      AtWorld -> (alterOrDelete (variableID vp) rwMValue env, localEnv)

alterOrDelete :: ID -> RW (Maybe Value) -> Env -> Env
alterOrDelete mID (R Nothing) env = error "[ERROR]<alterOrDelete>: Trying to delete R value"
alterOrDelete mID (W Nothing) env = IM.delete mID env
alterOrDelete mID (RW Nothing) env = IM.delete mID env
alterOrDelete mID (R (Just _)) env = error "[ERROR]<alterOrDelete>: Trying to set R value"
alterOrDelete mID (W (Just value)) env = IM.insert mID value env
alterOrDelete mID (RW (Just value)) env = IM.insert mID value env

runOperator :: CERES -> VVMap -> IO VVMap
runOperator anInstruction vvMap = return $
  case anInstruction of
    -- TODO: Need to check prior existence
    (InitVariable vp value) -> M.insert vp (W (Just value)) vvMap
    -- TODO: Need to check prior existence is R or RW
    (SetValue vp value) -> M.insert vp (W (Just value)) vvMap
    -- TODO: Need to check prior existence is R or RW
    (DeleteVariable vp) -> M.insert vp (W Nothing) vvMap
    -- TODO: Need to check prior existence is R or RW
    (ModifyValue vp operator) -> M.insert vp (RW (Just newValue)) vvMap
      where
        newValue :: Value
        newValue = case operator of
          COAMul -> caoMul
            (lookupVVMap vvMap vp operator)
            (lookupVVMap vvMap (VP 0 AtLocal) operator)
          (COAMulWith value) -> caoMul (lookupVVMap vvMap vp operator) value
          _ -> error $ "[ERROR]<runOperator>: Not yet implemented operator: " ++ show operator

lookupVVMap vvMap vp operator =
  (fromMaybe (errValueWith2 "takeOutFromVVMapLookup" "RefersDeletedVariable" vp operator)
    (runRW
      (fromMaybe (W (Just (errValueWith2 "takeOutFromVVMapLookup" "NotFound" vp operator))) (vvMap M.!? vp))))
