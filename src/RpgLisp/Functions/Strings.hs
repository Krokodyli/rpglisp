module RpgLisp.Functions.Strings where
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Functions.Helpers
import RpgLisp.Grammar
import Control.Monad


stringsFunctions :: [(String, ExprI)]
stringsFunctions = [
  ("cats", mkPrimitiveFunction $ simpleFunc concat extractString ExprString),
  ("show", mkPrimitiveFunction lispShow),
  ("words", mkPrimitiveFunction $ simpleOneArgFunc words extractString (ExprList . map ExprString)),
  ("unwords", mkPrimitiveFunction $ simpleOneArgFunc unwords (extractList >=> mapM extractString) ExprString),
  ("lines", mkPrimitiveFunction $ simpleOneArgFunc lines extractString (ExprList . map ExprString)),
  ("unlines", mkPrimitiveFunction $ simpleOneArgFunc unlines (extractList >=> mapM extractString) ExprString)]

lispShow :: [ExprI] -> Interpreter ExprI
lispShow [expr] = pure $ ExprString $ show expr
lispShow _ = interpreterThrow Error
