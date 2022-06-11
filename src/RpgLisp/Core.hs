module RpgLisp.Core where

import qualified Data.Map.Strict as M;
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Functions.Flow (controlFlowFunctions)
import RpgLisp.Functions.Math (mathFunctions)
import RpgLisp.Functions.Language (languageFunctions)
import RpgLisp.Functions.List (listFunctions)
import RpgLisp.Functions.AssocList (assocListFunctions)
import RpgLisp.Functions.Strings (stringsFunctions)

coreEnvironment :: InterpreterState
coreEnvironment = InterpreterState (M.fromList $ concat [
  languageFunctions,
  controlFlowFunctions,
  mathFunctions,
  listFunctions,
  assocListFunctions,
  stringsFunctions]) M.empty []
