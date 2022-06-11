{-# LANGUAGE LambdaCase #-}
module RpgLisp.Functions.Language where

import RpgLisp.Grammar
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Functions.Helpers
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import Data.Functor
import Data.Maybe
import Control.Lens

type FuncConstructor = [(String, ExprI)] -> [String] -> [ExprI] -> FuncDataI

languageFunctions :: [(String, ExprI)]
languageFunctions = [
  ("eval", mkPrimitiveFunction lispEval),
  ("quote", mkSpecialPrimitiveFunction lispQuote),
  ("def", mkSpecialPrimitiveFunction lispDef),
  ("def*", mkSpecialPrimitiveFunction lispDefStar),
  ("let", mkSpecialPrimitiveFunction lispLet),
  ("let*", mkSpecialPrimitiveFunction lispLetStar),
  ("defn", mkSpecialPrimitiveFunction lispDefn),
  ("defn*", mkSpecialPrimitiveFunction lispDefnStar),
  ("fn", mkSpecialPrimitiveFunction lispLambda),
  ("fn*", mkSpecialPrimitiveFunction lispLambdaStar),
  ("type-of", mkPrimitiveFunction lispTypeof),
  ("apply", mkPrimitiveFunction lispApply)]

lispEval :: [ExprI] -> Interpreter ExprI
lispEval [expr] = eval expr
lispEval _ = interpreterThrow InvalidArgs

lispQuote :: [ExprI] -> Interpreter ExprI
lispQuote [arg] = pure arg
lispQuote _ = interpreterThrow InvalidArgs

defineVariable :: String -> ExprI -> Interpreter ExprI
defineVariable name value = (globalEnv %= M.insert name value) $> ExprAtom name

defineLocalVariable :: String -> ExprI -> Interpreter ExprI
defineLocalVariable name value = (localEnv %= M.insert name value) $> ExprAtom name

lispDef :: [ExprI] -> Interpreter ExprI
lispDef [ExprAtom name, expr] = eval expr >>= defineVariable name
lispDef _ = interpreterThrow InvalidArgs

lispDefStar :: [ExprI] -> Interpreter ExprI
lispDefStar [nameExpr, expr] = eval nameExpr >>= (\case
  ExprAtom name -> eval expr >>= defineVariable name
  _ -> interpreterThrow InvalidArgs)
lispDefStar _ = interpreterThrow InvalidArgs

lispLet :: [ExprI] -> Interpreter ExprI
lispLet [ExprAtom name, expr] = eval expr >>= defineLocalVariable name
lispLet _ = interpreterThrow InvalidArgs

lispLetStar :: [ExprI] -> Interpreter ExprI
lispLetStar [nameExpr, expr] = eval nameExpr >>= (\case
  ExprAtom name -> eval expr >>= defineLocalVariable name
  _ -> interpreterThrow InvalidArgs)
lispLetStar _ = interpreterThrow InvalidArgs

lispDefn :: [ExprI] -> Interpreter ExprI
lispDefn (ExprAtom name : rest) = lispLambda rest >>= defineVariable name
lispDefn _ = interpreterThrow InvalidArgs

lispDefnStar :: [ExprI] -> Interpreter ExprI
lispDefnStar (ExprAtom name : rest) = lispLambdaStar rest >>= defineVariable name
lispDefnStar _ = interpreterThrow InvalidArgs

lispLambda :: [ExprI] -> Interpreter ExprI
lispLambda (ExprList nameList : body) = do
  names <- extractMany extractAtom nameList
  ExprFunc <$> createFunction Func names body
lispLambda _ = interpreterThrow InvalidArgs

lispLambdaStar :: [ExprI] -> Interpreter ExprI
lispLambdaStar (ExprList nameList : body) = do
  names <- extractMany extractAtom nameList
  ExprFunc <$> createFunction SpecialFunc names body
lispLambdaStar _ = interpreterThrow InvalidArgs

createFunction :: FuncConstructor -> [String] -> [ExprI] -> Interpreter FuncDataI
createFunction constructFunc argNames body@(_:_) = do
  iState <- get
  let closure = createClosure iState body
  pure $ constructFunc closure argNames body
createFunction _ _ _ = interpreterThrow InvalidArgs

removeDuplicates :: (Ord a) => [(a, b)] -> [(a, b)]
removeDuplicates = M.toList . M.fromList

createClosure :: InterpreterState -> [ExprI] -> [(String, ExprI)]
createClosure currentState funcBody = removeDuplicates validAtoms
  where
    outerClosure = view localEnv currentState
    atoms = concatMap findAtoms funcBody
    atomsWithValues = zip atoms (map (outerClosure M.!?) atoms)
    transformPair (k, v) = case v of
      Nothing -> Nothing
      Just justV -> Just (k, justV)
    validAtoms = mapMaybe transformPair atomsWithValues

findAtoms :: ExprI -> [String]
findAtoms (ExprAtom name) = [name]
findAtoms (ExprList list) = concatMap findAtoms list
findAtoms _ = []

lispApply :: [ExprI] -> Interpreter ExprI
lispApply [ExprFunc f, ExprList args] = applyFunction "<apply-func>" f args
lispApply _ = interpreterThrow InvalidArgs

lispTypeof :: [ExprI] -> Interpreter ExprI
lispTypeof [ExprInt _] = pure $ ExprString "int"
lispTypeof [ExprString _] = pure $ ExprString "string"
lispTypeof [ExprBool _] = pure $ ExprString "bool"
lispTypeof [ExprNil] = pure $ ExprString "nil"
lispTypeof [ExprAtom _] = pure $ ExprString "atom"
lispTypeof [ExprList _] = pure $ ExprString "list"
lispTypeof [ExprFunc _] = pure $ ExprString "func"
lispTypeof _ = interpreterThrow InvalidArgs
