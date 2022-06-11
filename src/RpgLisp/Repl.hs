module RpgLisp.Repl where

import RpgLisp.Interpreter.Interpreter
import qualified System.Console.Haskeline as HL
import RpgLisp.Grammar
import RpgLisp.Parser
import System.IO
import RpgLisp.GameEngine.GameEngine
import RpgLisp.Core
import Data.Functor
import Data.List
import Control.Monad.IO.Class

runParsed :: ParsedExpr -> InterpreterState -> Either String (ExprI, InterpreterState)
runParsed parsed iState = do
  case runInterpreter (convertParsed parsed) iState of
    (Left iError) -> Left $ showInterpreterError iError
    Right result -> Right result

runAllParsed :: [(ParsedExpr, Int)] -> InterpreterState -> Either String ([ExprI], InterpreterState)
runAllParsed [] iState = Right ([], iState)
runAllParsed ((expression, _):es) iState =
  case runParsed expression iState of
    Left iError -> Left iError
    Right (result, newState) -> do
      case runAllParsed es newState of
        Left iError -> Left iError
        Right (results, finalState) -> Right (result : results, finalState)

parseAndRun :: String -> InterpreterState -> Either String ([ExprI], InterpreterState)
parseAndRun code environment = do
  case parseManyExprWithLines code of
    Left pError -> Left pError
    Right parsed -> runAllParsed parsed environment

loadFile :: String -> InterpreterState -> IO InterpreterState
loadFile filename iState = do
  fileContents <- readFile' filename
  case parseAndRun fileContents iState of
    Left errMess -> putStrLn errMess $> iState
    Right (results, newState) -> mapM_ print results $> newState

runGameMode :: String -> IO ()
runGameMode filename = do
  fileContents <- readFile filename
  case parseAndRun fileContents coreEnvironment of
    Left errMess -> putStrLn errMess
    Right (_, initialState) -> do
      runGame initialState

runReplCommand :: InterpreterState -> String -> IO (Bool, InterpreterState)
runReplCommand iState cmd = case words cmd of
  [":l", filename] -> loadFile filename iState <&> (,) True
  [":q"] -> pure (False, iState)
  _ -> putStrLn "Invalid command" $> (True, iState)

replLoop :: InterpreterState -> IO ()
replLoop iState = HL.runInputT HL.defaultSettings (loop iState)
  where
    isReplCommand = isPrefixOf ":"
    loop :: InterpreterState -> HL.InputT IO ()
    loop loopState = do
      userInput <- HL.getInputLine "> "
      case userInput of
        Nothing -> return ()
        Just input -> do
          if isReplCommand input then do
            (status, newState) <- liftIO $ runReplCommand loopState input
            if status then loop newState else pure ()
          else case parseAndRun input loopState of
            Left iError -> HL.outputStrLn iError >> loop loopState
            Right (results, newState) -> do
              mapM_ (HL.outputStrLn . show) results >> loop newState

runReplMode :: InterpreterState -> IO ()
runReplMode = replLoop
