{-# LANGUAGE LambdaCase #-}
module RpgLisp.Functions.List where
import RpgLisp.Grammar
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Functions.Helpers
import RpgLisp.Functions.Math (areExprEqual)
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.Foldable (find)

listFunctions :: [(String, ExprI)]
listFunctions = mkPrimitiveFunctionList [
  ("list", lispList),
  ("cons", lispCons),
  ("car", lispCar),
  ("cdr", lispCdr),
  ("empty?", lispEmpty),
  ("length", simpleOneArgFunc length extractList ExprInt),
  ("cat", simpleFoldFunc (++) extractList ExprList),
  ("map", lispMap),
  ("filter", lispFilter),
  ("find", lispFind),
  ("insert", lispInsert),
  ("remove", lispRemove),
  ("member", lispMember)]

lispList :: [ExprI] -> Interpreter ExprI
lispList xs = pure $ ExprList xs

lispCons :: [ExprI] -> Interpreter ExprI
lispCons [listHead, ExprList rest] = pure $ ExprList (listHead:rest)
lispCons _ = interpreterThrow InvalidArgs

lispCar :: [ExprI] -> Interpreter ExprI
lispCar [ExprList (x:_)] = pure x
lispCar _ = interpreterThrow InvalidArgs

lispCdr :: [ExprI] -> Interpreter ExprI
lispCdr [ExprList (_:xs)] = pure $ ExprList xs
lispCdr _ = interpreterThrow InvalidArgs

lispEmpty :: [ExprI] -> Interpreter ExprI
lispEmpty = simpleOneArgFunc null extractList ExprBool

lispMap :: [ExprI] -> Interpreter ExprI
lispMap [ExprFunc func, ExprList list] = ExprList <$> result
  where
    applyFunctionWithOneArg x = applyFunction' "<map-func>" func [x]
    result = mapM applyFunctionWithOneArg list
lispMap _ = interpreterThrow InvalidArgs

extractPredicateResult :: ExprI -> Interpreter Bool
extractPredicateResult result = case extractBool result of
    Just val -> pure val
    Nothing -> interpreterThrow InvalidPredicateResult
    
lispFilter :: [ExprI] -> Interpreter ExprI
lispFilter [ExprFunc func, ExprList list] = ExprList <$> result
  where
    applyFunctionWithOneArg x = applyFunction' "<filter-func>" func [x]
    result = filterM (applyFunctionWithOneArg >=> extractPredicateResult) list
lispFilter _ = interpreterThrow InvalidArgs

insertElementInList :: ExprI -> ExprI -> ExprI
insertElementInList element (ExprList list) = ExprList $ element : list
insertElementInList _ a = a

removeFirstOccurrence :: (a -> Bool) -> [a] -> [a]
removeFirstOccurrence predicate (x:xs) =
  if predicate x
     then xs
     else x : removeFirstOccurrence predicate xs
removeFirstOccurrence _ [] = []

removeElementFromList :: ExprI -> ExprI -> ExprI
removeElementFromList itemToRemove (ExprList list) =
  ExprList $ removeFirstOccurrence isElemEqual list
  where
    isElemEqual item = fromMaybe False (areExprEqual itemToRemove item)
removeElementFromList _ a = a

lispInsert :: [ExprI] -> Interpreter ExprI
lispInsert [newItem, ExprList list] = pure $ ExprList (newItem : list)
lispInsert _ = interpreterThrow InvalidArgs

lispRemove :: [ExprI] -> Interpreter ExprI
lispRemove [itemToRemove, list@(ExprList _)] = pure $ removeElementFromList itemToRemove list
lispRemove _ = interpreterThrow InvalidArgs

lispMember :: [ExprI] -> Interpreter ExprI
lispMember [expr, ExprList elements] =
  pure $ ExprBool $ any (fromMaybe False . areExprEqual expr) elements
lispMember _ = interpreterThrow InvalidArgs

findElement :: FuncDataI -> [ExprI] -> Interpreter ExprI
findElement f [] = pure ExprNil
findElement f (x:xs) = do
  applyFunction' "<find-func>" f [x] >>= extractPredicateResult >>= (\case
    True -> pure x
    False -> findElement f xs)

lispFind :: [ExprI] -> Interpreter ExprI
lispFind [ExprFunc predicate, ExprList elements] =
  findElement predicate elements
lispFind _ = interpreterThrow InvalidArgs
