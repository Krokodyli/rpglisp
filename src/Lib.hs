module Lib
    ( main
    ) where

import RpgLisp.Repl
import System.Environment
import RpgLisp.Core
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runReplMode coreEnvironment
    [filename] -> loadFile filename coreEnvironment >>= (void . runReplMode)
    ["run", filename] -> void $ loadFile filename coreEnvironment
    ["game", filename] -> runGameMode filename
    _ -> putStrLn "Invalid arguments"
