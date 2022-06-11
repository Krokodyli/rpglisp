module RpgLisp.Interpreter.Internal.Runners where
import RpgLisp.Interpreter.Internal.Types
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import RpgLisp.Interpreter.Internal.Functions
import Control.Lens
import Data.List

runInterpreter :: ExprI -> InterpreterState -> Either InterpreterError (ExprI, InterpreterState)
runInterpreter expr = runExcept . runStateT (eval expr)

runInterpreterMany :: [ExprI] -> InterpreterState -> Either InterpreterError ([ExprI], InterpreterState)
runInterpreterMany expr = runExcept . runStateT (mapM eval expr)

runInterpreterIO :: Interpreter a -> InterpreterState -> IO (a, InterpreterState)
runInterpreterIO func iState = case runExcept (runStateT func iState) of
  (Left iError) -> error $ showInterpreterError iError
  (Right (value, newState)) -> pure (value, newState)

showInterpreterError :: InterpreterError -> String
showInterpreterError iError = unlines [messageStr, stackStr, localEnvStr]
  where
    messageStr = show $ view errorMessage iError
    stackStr = "STACK: " ++ intercalate ", " (view (errorInterpreterState . funcStack) iError)
    localEnvStr = "LOCAL ENV:" ++ show (view errorInterpreterState iError)
