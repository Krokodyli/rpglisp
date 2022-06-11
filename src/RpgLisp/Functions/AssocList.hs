module RpgLisp.Functions.AssocList where

import RpgLisp.Interpreter.Interpreter
import RpgLisp.Grammar
import RpgLisp.Functions.Language (defineVariable)
import RpgLisp.Functions.Helpers (mkPrimitiveFunction)
import Data.List (find)
import RpgLisp.Functions.Math (areExprEqual)
import Data.Maybe

assocListFunctions :: [(String, ExprI)]
assocListFunctions = [
  ("get", mkPrimitiveFunction lispGet),
  ("set", mkPrimitiveFunction lispSet),
  ("unset", mkPrimitiveFunction lispUnset)]

hasKey :: ExprI -> ExprI -> Bool
hasKey key (ExprList [nodeKey, _]) = fromMaybe False (areExprEqual key nodeKey)
hasKey _ _ = False

extractValueFromPair :: ExprI -> Maybe ExprI
extractValueFromPair (ExprList [_, val]) = Just val
extractValueFromPair _ = Nothing

createPair :: ExprI -> ExprI -> ExprI
createPair key value = ExprList [key, value]

getFromAssocList :: ExprI -> ExprI -> Maybe ExprI
getFromAssocList key (ExprList list) =
  find (hasKey key) list >>= extractValueFromPair
getFromAssocList _ _ = Nothing

unsetInAssocList :: ExprI -> ExprI -> ExprI
unsetInAssocList key (ExprList list) =
  ExprList $ filter (not . hasKey key) list
unsetInAssocList _ list = list

setInAssocList :: ExprI -> ExprI -> ExprI -> ExprI
setInAssocList key newValue (ExprList list) =
  ExprList $ if elementInList
     then snd changedExpr
     else createPair key newValue : list
  where
    changeFunc el (elemInList, rest) =
      if hasKey key el
        then (True, createPair key newValue : rest)
        else (elemInList, el : rest)
    changedExpr = foldr changeFunc (False, []) list
    elementInList = fst changedExpr
setInAssocList _ _ list = list

lispGet :: [ExprI] -> Interpreter ExprI
lispGet [key, list] =
  case getFromAssocList key list of
    Just a -> pure a
    Nothing -> pure ExprNil
lispGet _ = interpreterThrow InvalidArgs

lispSet :: [ExprI] -> Interpreter ExprI
lispSet [key, value, ExprList aList] = 
  pure $ setInAssocList key value (ExprList aList)
lispSet _ = interpreterThrow InvalidArgs

lispUnset :: [ExprI] -> Interpreter ExprI
lispUnset [key, list] = pure $ unsetInAssocList key list
lispUnset _ = interpreterThrow InvalidArgs
