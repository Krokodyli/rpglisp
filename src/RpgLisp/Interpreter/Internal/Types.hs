{-# LANGUAGE TemplateHaskell #-}
module RpgLisp.Interpreter.Internal.Types where

import RpgLisp.Grammar
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Lens

type Interpreter = StateT InterpreterState (Except InterpreterError)
type ExprI = Expr Interpreter

data InterpreterState = InterpreterState {
  _globalEnv :: M.Map String ExprI,
  _localEnv :: M.Map String ExprI,
  _funcStack :: [String] } deriving Show

data InterpreterErrorMessage = VariableNotFound String
                             | InvalidArgs
                             | InvalidOperator
                             | UnexpectedEmptyExpression
                             | InvalidPredicateResult
                             | Error
                             | Error2 deriving Show

data InterpreterError = InterpreterError {
  _errorMessage :: InterpreterErrorMessage,
  _errorInterpreterState :: InterpreterState }

type FuncDataI = FuncData Interpreter

$(makeLenses ''InterpreterState)
$(makeLenses ''InterpreterError)

pushFuncToStack :: String -> Interpreter ()
pushFuncToStack funcName = funcStack %= (funcName:)

popFuncFromStack :: Interpreter ()
popFuncFromStack = funcStack %= safeTail
  where
    safeTail :: [a] -> [a]
    safeTail [] = []
    safeTail (_:xs) = xs
