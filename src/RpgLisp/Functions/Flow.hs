{-# LANGUAGE LambdaCase #-}
module RpgLisp.Functions.Flow where

import RpgLisp.Grammar
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Functions.Helpers

controlFlowFunctions :: [(String, ExprI)]
controlFlowFunctions = [
  ("if", mkSpecialPrimitiveFunction lispIf),
  ("progn", mkSpecialPrimitiveFunction lispProgn),
  ("when", mkSpecialPrimitiveFunction lispWhen),
  ("unless", mkSpecialPrimitiveFunction lispUnless),
  ("cond", mkSpecialPrimitiveFunction condFunc)]

lispProgn :: [ExprI] -> Interpreter ExprI
lispProgn body@(_:_) = last <$> mapM eval body
lispProgn _ = interpreterThrow UnexpectedEmptyExpression

ifFunc :: ExprI -> ExprI -> ExprI -> Interpreter ExprI
ifFunc cond trueCase falseCase = eval cond >>= (\case
  Just True -> eval trueCase
  Just False -> eval falseCase
  Nothing -> interpreterThrow InvalidArgs) . extractBool

lispIf :: [ExprI] -> Interpreter ExprI
lispIf [cond, trueCase, falseCase] = ifFunc cond trueCase falseCase
lispIf _ = interpreterThrow InvalidArgs

lispWhen :: [ExprI] -> Interpreter ExprI
lispWhen [cond, trueCase] = ifFunc cond trueCase ExprNil
lispWhen _ = interpreterThrow InvalidArgs

lispUnless :: [ExprI] -> Interpreter ExprI
lispUnless [cond, falseCase] = ifFunc cond ExprNil falseCase
lispUnless _ = interpreterThrow InvalidArgs

condFunc :: [ExprI] -> Interpreter ExprI
condFunc [] = pure ExprNil
condFunc (x:xs) = do
  evalTest x >>= (\case
    True -> evalAction x
    False -> condFunc xs)
  where
    evalTest :: ExprI -> Interpreter Bool
    evalTest (ExprList [test, _]) = eval test >>= extract extractBool
    evalTest _ = interpreterThrow InvalidArgs
    evalAction :: ExprI -> Interpreter ExprI
    evalAction (ExprList [_, action]) = eval action
    evalAction _ = interpreterThrow InvalidArgs
