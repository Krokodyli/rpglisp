module RpgLisp.Functions.Helpers where

import RpgLisp.Grammar
import RpgLisp.Interpreter.Interpreter
import Data.Functor
import Data.Foldable
import Control.Arrow

mkSpecialPrimitiveFunction :: ([ExprI] -> Interpreter ExprI) -> ExprI
mkSpecialPrimitiveFunction = ExprFunc . PrimitiveFunc

evaluateAllArgs :: ([ExprI] -> Interpreter ExprI) -> ([ExprI] -> Interpreter ExprI)
evaluateAllArgs f args = mapM eval args >>= f

mkPrimitiveFunction :: ([ExprI] -> Interpreter ExprI) -> ExprI
mkPrimitiveFunction = mkSpecialPrimitiveFunction . evaluateAllArgs

mkPrimitiveFunctionList :: [(String, [ExprI] -> Interpreter ExprI)] -> [(String, ExprI)]
mkPrimitiveFunctionList = map (second mkPrimitiveFunction)

extract :: (ExprI -> Maybe a) -> ExprI -> Interpreter a
extract unpack expr = case unpack expr of
  Just value -> pure value
  Nothing -> interpreterThrow InvalidArgs

extractMany :: (ExprI -> Maybe a) -> [ExprI] -> Interpreter [a]
extractMany extractFunc args = maybe (interpreterThrow Error) pure extractedVals
  where
    extractedVals = sequence $ extractFunc <$> args

simpleFunc :: ([a] -> b) -> (ExprI -> Maybe a) -> (b -> ExprI) -> [ExprI] -> Interpreter ExprI
simpleFunc f unpack pack args = extractMany unpack args <&> (pack . f)

simpleFoldFunc :: (a -> a -> a) -> (ExprI -> Maybe a) -> (a -> ExprI) -> [ExprI] -> Interpreter ExprI
simpleFoldFunc f unpack pack (firstArg:rest) = do
  extractedHead <- extract unpack firstArg
  extractedArgs <- extractMany unpack rest
  pure $ pack $ foldl' f extractedHead extractedArgs
simpleFoldFunc _ _ _ _ = interpreterThrow InvalidArgs

compareFunc :: (a -> a -> Bool) -> (ExprI -> Maybe a) -> [ExprI] -> Interpreter ExprI
compareFunc comp unpack (firstArg:rest) = do
  extractedHead <- extract unpack firstArg
  extractedArgs <- extractMany unpack rest
  pure $ ExprBool $ all (comp extractedHead) extractedArgs
compareFunc _ _ _ = interpreterThrow InvalidArgs

simpleOneArgFunc :: (a -> b) -> (ExprI -> Maybe a) -> (b -> ExprI) -> [ExprI] -> Interpreter ExprI
simpleOneArgFunc func unpack pack [arg] = extract unpack arg <&> (pack . func)
simpleOneArgFunc _ _ _ _ = interpreterThrow InvalidArgs
