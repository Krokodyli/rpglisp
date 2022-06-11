{-# LANGUAGE LambdaCase #-}
module RpgLisp.GameEngine.GameEngine where

import RpgLisp.Grammar
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Functions.Language
import Control.Monad
import qualified System.Console.Haskeline as HL
import Data.Foldable
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

runGame :: InterpreterState -> IO ()
runGame initialState = do
  stateAfterInit <- runInit initialState
  HL.runInputT HL.defaultSettings $ gameLoop stateAfterInit

gameLoop :: InterpreterState -> HL.InputT IO ()
gameLoop iState = do
  runningStatus <- getRunningStatus iState
  getOutput iState >>= traverse_ HL.outputStr
  when runningStatus $
    HL.getInputLine "> " >>= traverse_ (\input ->
      setInput input iState >>= update >>= gameLoop)

initFunctionName :: String
initFunctionName = "init"

isRunningVariableName :: String
isRunningVariableName = "is-running"

inputVariableName :: String
inputVariableName = "input"

outputVariableName :: String
outputVariableName = "output"

updateFunctionName :: String
updateFunctionName = "update"

runInit :: InterpreterState -> IO InterpreterState
runInit iState = snd <$> runInterpreterIO runInit' iState
  where
    runInit' :: Interpreter ()
    runInit' = do
      lookupInEnv initFunctionName >>= (\case
        Just (ExprFunc initFunc) -> applyFunction initFunctionName initFunc [] >> pure ()
        _ -> pure ())

getRunningStatus :: InterpreterState -> HL.InputT IO Bool
getRunningStatus iState = lift $ fst <$> runInterpreterIO getRunningStatus' iState
  where
    getRunningStatus' :: Interpreter Bool
    getRunningStatus' = do
      lookupInEnv isRunningVariableName >>= (\case
        Just (ExprBool status) -> pure status
        _ -> pure True)

update :: InterpreterState -> HL.InputT IO InterpreterState
update iState = lift $ snd <$> runInterpreterIO update' iState
  where
    update' :: Interpreter ()
    update' = do
      lookupInEnv updateFunctionName >>= (\case
        Just (ExprFunc updateFunc) -> applyFunction updateFunctionName updateFunc [] >> pure ()
        _ -> pure ())

setInput :: String -> InterpreterState -> HL.InputT IO InterpreterState
setInput userInput iState = lift $ snd <$> runInterpreterIO (setInput' userInput) iState
  where
    setInput' :: String -> Interpreter ()
    setInput' input = defineVariable inputVariableName (ExprString input) >> pure ()

getOutput :: InterpreterState -> HL.InputT IO (Maybe String)
getOutput iState = lift $ fst <$> runInterpreterIO getOutput' iState
  where
    getOutput' :: Interpreter (Maybe String)
    getOutput' = lookupInEnv outputVariableName >>= (\case
      Just (ExprString "") -> pure Nothing
      Just (ExprString output) -> pure $ Just output
      _ -> pure Nothing)
