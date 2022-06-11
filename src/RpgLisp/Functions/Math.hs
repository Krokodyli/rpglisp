module RpgLisp.Functions.Math where

import RpgLisp.Interpreter.Interpreter
import RpgLisp.Grammar
import RpgLisp.Functions.Helpers
import Control.Monad
import Data.Maybe

mathFunctions :: [(String, ExprI)]
mathFunctions = mkPrimitiveFunctionList [
  ("eq?", eqFunc),
  ("+", simpleFunc sum extractInt ExprInt),
  ("*", simpleFunc product extractInt ExprInt),
  ("-", simpleFoldFunc (-) extractInt ExprInt),
  ("/", simpleFoldFunc div extractInt ExprInt),
  ("mod", simpleFoldFunc mod extractInt ExprInt),
  ("and", simpleFunc and extractBool ExprBool),
  ("or", simpleFunc or extractBool ExprBool),
  ("not", lispNot),
  (">", compareFunc (>) extractInt),
  ("<", compareFunc (<) extractInt),
  (">=", compareFunc (>=) extractInt),
  ("<=", compareFunc (<=) extractInt),
  ("=", compareFunc (==) extractInt),
  ("!=", compareFunc (/=) extractInt)]

eqFunc :: [ExprI] -> Interpreter ExprI
eqFunc (firstArg:rest) = foldM evalEquality firstArg rest
  where
    evalEquality a b = ExprBool <$> areExprEqualFunc a b
eqFunc _ = interpreterThrow Error

areExprEqualFunc :: ExprI -> ExprI -> Interpreter Bool
areExprEqualFunc x y = pure $ fromMaybe False $ areExprEqual x y

areExprEqual :: ExprI -> ExprI -> Maybe Bool
areExprEqual (ExprInt a) (ExprInt b) = Just $ a == b
areExprEqual (ExprString a) (ExprString b) = Just $ a == b
areExprEqual (ExprBool a) (ExprBool b) = Just $ a == b
areExprEqual ExprNil ExprNil = Just True
areExprEqual (ExprList a) (ExprList b) = and <$> zipWithM areExprEqual a b
areExprEqual (ExprAtom a) (ExprAtom b) = pure $ a == b
areExprEqual (ExprFunc _) _ = Nothing
areExprEqual _ (ExprFunc _) = Nothing
areExprEqual _ _ = pure False

lispNot :: [ExprI] -> Interpreter ExprI
lispNot [ExprBool b] = pure $ ExprBool $ not b
lispNot _ = interpreterThrow Error
