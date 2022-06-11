{-# LANGUAGE LambdaCase #-}
module RpgLisp.Interpreter.Internal.Functions where

import RpgLisp.Interpreter.Internal.Types
import qualified Data.Map.Strict as M
import RpgLisp.Grammar
import Control.Monad.Trans.State.Strict ( get )
import Control.Lens ( use, uses )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.Except ( throwE )
import Data.Foldable ( Foldable(foldl') )
import Control.Arrow ( Arrow(second) )
import Data.Functor ( ($>) )
import Control.Lens.Operators

eval :: ExprI -> Interpreter ExprI
eval (ExprList list) = evalList list
eval (ExprAtom atom) = findInEnv atom
eval expr = pure expr

evalList :: [ExprI] -> Interpreter ExprI
evalList (ExprAtom funcName : operands) = findInEnv funcName >>= \case
    (ExprFunc func) -> applyFunction funcName func operands
    _ -> interpreterThrow InvalidOperator
evalList (operator : operands) = eval operator >>= \case
    (ExprFunc func) -> applyFunction "<lambda>" func operands
    _ -> interpreterThrow InvalidOperator
evalList _ = interpreterThrow InvalidOperator

modifySymbolsInLocalEnv :: [(String, Maybe ExprI)] -> Interpreter ()
modifySymbolsInLocalEnv env = localEnv %= applyEnv' env
  where
    insertValuePair m (k, v) = maybe (M.delete k m) (\oldVal -> M.insert k oldVal m) v
    applyEnv' = flip $ foldl' insertValuePair

createFuncEnv :: FuncDataI -> [ExprI] -> [(String, ExprI)]
createFuncEnv (Func closure argNames _) args = closure ++ zip argNames args ++ [("_args", ExprList args)]
createFuncEnv (SpecialFunc closure argNames _) args = closure ++ zip argNames args ++ [("_args", ExprList args)]
createFuncEnv (PrimitiveFunc _) _ = []

runWithEnv :: [(String, ExprI)] -> [ExprI] -> Interpreter ExprI
runWithEnv env funcBody = do
  let names = map fst env
  oldLocalEnv <- use localEnv
  let oldValues = zip names $ map (oldLocalEnv M.!?) names
  modifySymbolsInLocalEnv $ map (second Just) env
  result <- last <$> mapM eval funcBody
  modifySymbolsInLocalEnv oldValues
  pure result

applyFunction' :: String -> FuncDataI -> [ExprI] -> Interpreter ExprI
applyFunction' funcName func@(Func _ _ funcBody) args = do
  pushFuncToStack funcName
  oldLocalEnv <- use localEnv
  let funcEnv = createFuncEnv func args
  result <- runWithEnv funcEnv funcBody
  localEnv .= oldLocalEnv
  popFuncFromStack $> result
applyFunction' funcName func@(SpecialFunc _ _ funcBody) args = do
  pushFuncToStack funcName
  oldLocalEnv <- use localEnv
  let funcEnv = createFuncEnv func args
  result <- runWithEnv funcEnv funcBody
  localEnv .= oldLocalEnv
  popFuncFromStack $> result
applyFunction' _ (PrimitiveFunc f) args = f args

applyFunction :: String -> FuncDataI -> [ExprI] -> Interpreter ExprI
applyFunction funcName func@(Func _ _ funcBody) args = do
  evaluatedArgs <- mapM eval args
  oldLocalEnv <- use localEnv
  let funcEnv = createFuncEnv func evaluatedArgs
  pushFuncToStack funcName
  returnVal <- runWithEnv funcEnv funcBody
  localEnv .= oldLocalEnv
  popFuncFromStack $> returnVal
applyFunction funcName func@(SpecialFunc {}) args = applyFunction' funcName func args
applyFunction funcName (PrimitiveFunc f) args = do
  pushFuncToStack funcName
  returnVal <- f args
  popFuncFromStack $> returnVal

lookupInLocalEnv :: String -> Interpreter (Maybe ExprI)
lookupInLocalEnv name = uses localEnv (M.!? name)

lookupInGlobalEnv :: String -> Interpreter (Maybe ExprI)
lookupInGlobalEnv name = uses globalEnv (M.!? name)

lookupInEnv :: String -> Interpreter (Maybe ExprI)
lookupInEnv name = lookupInLocalEnv name >>= (\case
  Just value -> pure $ Just value
  Nothing -> lookupInGlobalEnv name)

findInEnv :: String -> Interpreter ExprI
findInEnv name = lookupInEnv name >>= (\case
  (Just value) -> pure value
  _ -> interpreterThrow $ VariableNotFound name)

interpreterThrow :: InterpreterErrorMessage -> Interpreter a
interpreterThrow errorMess =
  get >>= (lift . throwE) . InterpreterError errorMess
