module CERES.Advanced.Interpret where


import qualified Data.Map                      as M
import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import qualified Data.Set                      as S
import           Data.Maybe

import           TextShow

import           CERES.Advanced.Operate
import           Data.CERES.Advanced.Script
import           Data.CERES.Advanced.Operator
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.Value.Error


-- TODO: How to distinguish
--  * 1. Referred, Added, or Changed
--  * 2. Referred, but deleted

-- TODO: Value -> ValueContainer
type Env = IM.IntMap Value
viewEnv :: Name -> Env -> IO ()
viewEnv envName = mapM_ (\x -> T.putStrLn . toText $ fromLazyText envName <> showb x)

type EnvSet = (Env, Env)
type VV = (VPosition, RW (Maybe Value))
type VVMap = M.Map VPosition (RW (Maybe Value))

data RW a = R a | W a | RW a deriving (Eq, Ord)
runRW :: RW a -> a
runRW (R  a) = a
runRW (W  a) = a
runRW (RW a) = a
notR :: RW a -> Bool
notR (R _) = False
notR _     = True

runCERES :: CEREScript -> EnvSet -> IO EnvSet
runCERES []                   (env, localEnv) = return (env, localEnv)
runCERES (aCERES : ceresList) (env, localEnv) = do
  T.putStrLn  "----------------------------------------------------------------"
  (newEnv, newLocalEnv) <- interpretCERES aCERES (env, localEnv)
  viewEnv "GEnv" newEnv
  T.putStrLn "-----------------"
  viewEnv "LEnv" newLocalEnv
  runCERES ceresList (newEnv, newLocalEnv)

interpretCERES :: CERES -> EnvSet -> IO EnvSet
interpretCERES anInstruction envSet = do
  let rvMap = readByInstruction envSet anInstruction M.empty
  vvMap <- runOperator anInstruction rvMap
  return $ setValuesToEnvSet envSet (filter (notR . snd) . M.toList $ vvMap)

-- TODO: Should be recursively
readByInstruction :: EnvSet -> CERES -> VVMap -> VVMap
readByInstruction envSet anInstruction rvMap = case anInstruction of
  (InitVariable _ _) -> rvMap
  (SetValue     _ _) -> rvMap
  (DeleteVariable _) -> rvMap
  -- TODO: Should be more complicate form
  (ModifyValue vpA vpB _) ->
    readValueFromEnvSet envSet rvMap [vpA, VP 0 AtLocal voidHere]
  (CopyValue _ _) -> error "[FIXME]<readByInstruction :=: CopyValue> Not yet implemented"
  (ConvertValue _ _) -> error "[FIXME]<readByInstruction :=: ConvertValue> Not yet implemented"
  (ConvertValueBy _ _) -> error "[FIXME]<readByInstruction :=: ConvertValueBy> Not yet implemented"

readValueFromEnvSet :: EnvSet -> VVMap -> [VPosition] -> VVMap
readValueFromEnvSet _ vvMap [] = vvMap
readValueFromEnvSet envSet@(env, localEnv) vvMap (vp : rest) =
  readValueFromEnvSet envSet (M.insert vp (R (Just theValue)) vvMap) rest
 where
  mValue = case variablePlace vp of
    AtWorld -> IM.lookup (variableID vp) env
    AtDict  -> error "[FIXME]<readValueFromEnvSet :=: AtDict>  Not yet implemented"
    AtVar   -> error "[FIXME]<readValueFromEnvSet :=: AtVar>   Not yet implemented"
    AtTime  -> error "[FIXME]<readValueFromEnvSet :=: AtTime>  Not yet implemented"
    AtLocal -> IM.lookup (variableID vp) localEnv
    AtCache -> error "[FIXME]<readValueFromEnvSet :=: AtCache> Not yet implemented"
    AtHere  -> error "[FIXME]<readValueFromEnvSet :=: AtHere>  Not yet implemented"
    _       -> error "[ERROR]<readValueFromEnvSet> No such VariablePlace"
  theValue = fromMaybe
    (ErrValue $ TL.append "readValueFromEnvSet - NotFound at " $ showtl vp)
    mValue

setValuesToEnvSet :: EnvSet -> [VV] -> EnvSet
setValuesToEnvSet envSet          []                      = envSet
setValuesToEnvSet (env, localEnv) ((vp, rwMValue) : rest) = setValuesToEnvSet
  (newEnv, newLocalEnv)
  rest
 where
  (newEnv, newLocalEnv) = case variablePlace vp of
    AtWorld -> (alterOrDelete (variableID vp) rwMValue env, localEnv)
    AtDict  -> error "[FIXME]<readValueFromEnvSet :=: AtDict>  Not yet implemented"
    AtVar   -> error "[FIXME]<readValueFromEnvSet :=: AtVar>   Not yet implemented"
    AtTime  -> error "[FIXME]<readValueFromEnvSet :=: AtTime>  Not yet implemented"
    AtLocal -> (env, alterOrDelete (variableID vp) rwMValue localEnv)
    AtCache -> error "[FIXME]<readValueFromEnvSet :=: AtCache> Not yet implemented"
    AtHere  -> error "[FIXME]<readValueFromEnvSet :=: AtHere>  Not yet implemented"
    _       -> error "[ERROR]<setValuesToEnvSet> No such VariablePlace"

alterOrDelete :: ID -> RW (Maybe Value) -> Env -> Env
alterOrDelete mID (R Nothing) env =
  error "[ERROR]<alterOrDelete>: Trying to delete R value"
alterOrDelete mID (W  Nothing) env = IM.delete mID env
alterOrDelete mID (RW Nothing) env = IM.delete mID env
alterOrDelete mID (R (Just _)) env =
  error "[ERROR]<alterOrDelete>: Trying to set R value"
alterOrDelete mID (W  (Just value)) env = IM.insert mID value env
alterOrDelete mID (RW (Just value)) env = IM.insert mID value env

runOperator :: CERES -> VVMap -> IO VVMap
runOperator anInstruction vvMap = return $ case anInstruction of
  -- TODO: Need to check prior existence
  (InitVariable vpA (VP _ AtHere value)) -> M.insert vpA (W (Just value)) vvMap
  (InitVariable vpA (VP _ _ value)) ->
    error
      "[ERROR]<runOperator :=: InitVariable> second argument's `VariablePlace` is not `AtHere`"
  -- TODO: Need to check prior existence is R or RW
  (SetValue vpA (VP _ AtHere value)) -> M.insert vpA (W (Just value)) vvMap
  (SetValue vpA vpB                ) -> M.insert vpA (W (Just value)) vvMap
    where value = error "[TODO]<runOperator :=: SetValue> read value from vpB"
  -- TODO: Need to check prior existence is R or RW
  (DeleteVariable vp           ) -> M.insert vp (W Nothing) vvMap
  -- TODO: Need to check prior existence is R or RW
  (ModifyValue vpA vpB operator) -> M.insert vpA (RW (Just newValue)) vvMap
   where
    newValue :: Value
    newValue = case operator of
      COAMul ->
        coaMul (lookupVVMap vvMap vpA operator) (lookupVVMap vvMap vpB operator)
      _ ->
        error
          $  "[FIXME]<runOperator>: Not yet implemented operator: "
          ++ show operator
  (CopyValue _ _) -> error "[FIXME]<runOperator :=: CopyValue> Not yet implemented"
  (ConvertValue _ _) -> error "[FIXME]<runOperator :=: ConvertValue> Not yet implemented"
  (ConvertValueBy _ _) -> error "[FIXME]<runOperator :=: ConvertValueBy> Not yet implemented"

lookupVVMap vvMap vp operator = fromMaybe
  (errValueWith2 "takeOutFromVVMapLookup" "RefersDeletedVariable" vp operator)
  (runRW
    (fromMaybe
      (W (Just (errValueWith2 "takeOutFromVVMapLookup" "NotFound" vp operator)))
      (vvMap M.!? vp)
    )
  )


type RWVPSet = S.Set (RW VPosition)
lookupRWVP :: CEREScript -> RWVPSet -> RWVPSet
lookupRWVP []                        aSet = aSet
lookupRWVP (anInstruction : aScript) aSet = case anInstruction of
  (InitVariable vp _    ) -> S.insert (W vp) aSet
  (SetValue     vp _    ) -> S.insert (W vp) aSet
  (DeleteVariable vp    ) -> S.insert (W vp) aSet
  -- TODO: Should be more complicate form
  (ModifyValue vpA vpB _) -> S.insert (W vpA) aSet
  (CopyValue _ _) -> error "[FIXME]<lookupRWVP :=: CopyValue> Not yet implemented"
  (ConvertValue _ _) -> error "[FIXME]<lookupRWVP :=: ConvertValue> Not yet implemented"
  (ConvertValueBy _ _) -> error "[FIXME]<lookupRWVP :=: ConvertValueBy> Not yet implemented"
