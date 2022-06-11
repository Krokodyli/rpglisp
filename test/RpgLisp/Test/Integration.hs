module RpgLisp.Test.Integration where

import Test.Tasty
import RpgLisp.Parser (parseManyExprWithLines)
import RpgLisp.Repl (runAllParsed)
import RpgLisp.Interpreter.Interpreter
import RpgLisp.Core (coreEnvironment)
import Test.Tasty.HUnit
import RpgLisp.Grammar

integrationTests :: TestTree
integrationTests = testGroup "Integration" [
  testGroup "Basic evaluation" [
      integrationTestCase "Integer" "1" [ExprInt 1]
    , integrationTestCase "Negative integer" "-1" [ExprInt $ -1]
    , integrationTestCase "String" "\"string\"" [ExprString "string"]
    , integrationTestCase "Bool false" "#f" [ExprBool False]
    , integrationTestCase "Bool true" "#t" [ExprBool True]
    , integrationTestCase "Nil" "#nil" [ExprNil]
  ]
  , testGroup "Arithmetic operations" [
      integrationTestCase "Adding" "(+ 15 -10)" [ExprInt 5]
    , integrationTestCase "Adding 2" "(+ 15 -10 10)" [ExprInt 15]
    , integrationTestCase "Subtracting" "(- 10 0)" [ExprInt 10]
    , integrationTestCase "Subtracting 2" "(- 0 -10 -20)" [ExprInt 30]
    , integrationTestCase "Multiplying" "(* 3 -3)" [ExprInt $ -9]
    , integrationTestCase "Multiplying 2" "(* 3 -3 2)" [ExprInt $ -18]
    , integrationTestCase "Dividing" "(/ 3 -3)" [ExprInt $ -1]
    , integrationTestCase "Dividing 2" "(/ 100 20 5)" [ExprInt 1]
    , integrationTestCase "Modulo 1" "(mod 22 7)" [ExprInt 1]
    , integrationTestCase "Modulo 2" "(mod 3 2)" [ExprInt 1]
  ]

  , testGroup "Comparison" [
      integrationTestCase "Operator > 1" "(> 5 5)" [ExprBool False]
    , integrationTestCase "Operator > 2" "(> 7 5 4 3)" [ExprBool True]
    , integrationTestCase "Operator < 1" "(< 7 8 9 1)" [ExprBool False]
    , integrationTestCase "Operator < 2" "(< 4 5)" [ExprBool True]
    , integrationTestCase "Operator <= 1" "(<= 4 5 4)" [ExprBool True]
    , integrationTestCase "Operator <= 2" "(<= 4 3)" [ExprBool False]
    , integrationTestCase "Operator >= 1" "(>= 7 5 4)" [ExprBool True]
    , integrationTestCase "Operator >= 2" "(>= 2 3)" [ExprBool False]
    , integrationTestCase "Operator = 1" "(= 4 4 4)" [ExprBool True]
    , integrationTestCase "Operator = 2" "(= 4 4 8)" [ExprBool False]
    , integrationTestCase "Operator != 1" "(!= 4 4 4)" [ExprBool False]
    , integrationTestCase "Operator != 2" "(!= 4 40 8)" [ExprBool True]
  ]

  , testGroup "Control flow functions" [
      integrationTestCase "Simple if 1" "(if (> 2 1) 3 4)" [ExprInt 3]
    , integrationTestCase "Simple if 1" "(if (< 2 1) 3 4)" [ExprInt 4]
    , integrationTestCase "Simple when 1" "(when (> 2 1) 3)" [ExprInt 3]
    , integrationTestCase "Simple when 2" "(when (< 2 1) 3)" [ExprNil]
    , integrationTestCase "Simple unless 1" "(unless (> 2 1) 3)" [ExprNil]
    , integrationTestCase "Simple unless 2" "(unless (< 2 1) 3)" [ExprInt 3]
    , integrationTestCase "Evaluation order" "(if #t 3 x)" [ExprInt 3]
  ]

  , testGroup "Logical operators" [
      integrationTestCase "and 1" "(and #t #t #t #t #t)" [ExprBool True]
    , integrationTestCase "and 2" "(and #t #t #f #t)" [ExprBool False]
    , integrationTestCase "or 1" "(or #f #f)" [ExprBool False]
    , integrationTestCase "or 2" "(or #f #f #t #f)" [ExprBool True]
    , integrationTestCase "not 1" "(not #f)" [ExprBool True]
    , integrationTestCase "not 2" "(not #t)" [ExprBool False]
  ]

  , testGroup "Quoting" [
      integrationTestCase "Quoting atoms" "'x" [ExprAtom "x"]
    , integrationTestCase "Quoting lists" "'(1 2 3)" [ExprList $ ExprInt <$> [1, 2, 3]]
  ]

  , testGroup "Global variables" [
      integrationTestCase "Defining variables" "(def x \"s\") x" [ExprAtom "x", ExprString "s"]
    , integrationTestCase "Defining variables 2" "(def x (+ 2 3)) x" [ExprAtom "x", ExprInt 5]
    , integrationTestCase "Defining variables 2" "(def x (+ 2 3)) x" [ExprAtom "x", ExprInt 5]
    , integrationTestCase "Defining variables 3" "(def x 'atom) x" [ExprAtom "x", ExprAtom "atom"]
    , integrationTestCase "Defining variables 4" "(def x 'var) (def* x 2) var" [ExprAtom "x", ExprAtom "var", ExprInt 2]
  ]

  , testGroup "Basic functions" [
      integrationTestCase "Defining function" "(defn add (x y) (+ x y)) \
                        \ (add 5 -1)" [ExprAtom "add", ExprInt 4]
    , integrationTestCase "Using lambda" "((fn (x) (* x 2)) 5)" [ExprInt 10]
  ]

  , testGroup "Functions that use state" [
      integrationTestCase "Defining function that changes global state"
        "(defn change-state (x) (def y x)) \
      \ (change-state \"hello\") y" [ExprAtom "change-state", ExprAtom "y", ExprString "hello"]

    , integrationTestCase "Defining function that returns function that uses local state"
        "(defn adder (x) (fn (y) (+ x y))) \
      \ ((adder 3) 5)" [ExprAtom "adder", ExprInt 8]

    , integrationTestCase "Using function in another function body"
        "(defn square (x) (* x x)) \
      \ (defn squareDiff (x y) (- (square x) (square y)))\
      \ (squareDiff 5 4)" [ExprAtom "square", ExprAtom "squareDiff", ExprInt 9]
    ]

  , testGroup "Defining local variables" [
      integrationTestCase "Defining local variables"
                          "(def x 3) \
                        \ (defn f () (let x 2) x) (f) x"
                          [ExprAtom "x", ExprAtom "f", ExprInt 2, ExprInt 3]

    , integrationTestCase "Defining local variables 2"
                          "(def x 1)\
                        \ (defn f () (let x 2) x)\
                        \ (defn g () (let x 3) (+ (f) x))\
                        \ (+ x (g))"
                          [ExprAtom "x", ExprAtom "f", ExprAtom "g", ExprInt 6]
    , integrationTestCase "Defining local variables 3"
                          "(defn f () (let x 'atom) (let* x 3) atom) (f)"
                        [ExprAtom "f", ExprInt 3]
  ]

  , testGroup "Association list" [
      integrationTestCase "Get 1"
                          "(def x '((1 2) (2 4) (4 8)))\
                        \  (get 1 x)"
                          [ExprAtom "x", ExprInt 2]
    , integrationTestCase "Get 2"
                          "(def x '((a 2) (b 4) (c 8)))\
                        \  (get 'c x)"
                          [ExprAtom "x", ExprInt 8]
    , integrationTestCase "Get 3"
                          "(def x '((a 2) (b 4) (c 8)))\
                        \  (get 'd x)"
                          [ExprAtom "x", ExprNil]
    , integrationTestCase "Get 4"
                          "(def x '())\
                        \  (get 'a x)"
                          [ExprAtom "x", ExprNil]
    , integrationTestCase "Set 1"
                          "(def x '())\
                        \  (set 'a 2 x)"
                          [ExprAtom "x", ExprList [ExprList [ExprAtom "a", ExprInt 2]]]
    , integrationTestCase "Set 2"
                          "(def x '((a 4) (b 3)))\
                        \  (set 'b 2 x)"
                          [ExprAtom "x", 
                           ExprList [ExprList [ExprAtom "a", ExprInt 4],
                                     ExprList [ExprAtom "b", ExprInt 2]]]
    , integrationTestCase "Set 3"
                          "(def x '((a 4) (b 3)))\
                        \  (set 'c 2 x)"
                          [ExprAtom "x", 
                           ExprList [ExprList [ExprAtom "c", ExprInt 2],
                                     ExprList [ExprAtom "a", ExprInt 4],
                                     ExprList [ExprAtom "b", ExprInt 3]]]
    , integrationTestCase "Unset 1"
                          "(def x '((1 2) (2 4) (4 8)))\
                        \  (unset 1 x)"
                          [ExprAtom "x",
                           ExprList [ExprList [ExprInt 2, ExprInt 4],
                                     ExprList [ExprInt 4, ExprInt 8]]]
    , integrationTestCase "Unset 2"
                          "(def x '((\"c\" 2) (\"b\" 4) (\"a\" 8)))\
                        \  (unset \"a\" x)"
                          [ExprAtom "x",
                           ExprList [ExprList [ExprString "c", ExprInt 2],
                                     ExprList [ExprString "b", ExprInt 4]]]
    , integrationTestCase "Unset 3"
                          "(def x '((a 2) (b 4) (c 8)))\
                        \  (unset 'd x)"
                          [ExprAtom "x",
                           ExprList [ExprList [ExprAtom "a", ExprInt 2],
                                     ExprList [ExprAtom "b", ExprInt 4],
                                     ExprList [ExprAtom "c", ExprInt 8]]]
  ]

  , testGroup "List functions" [
     integrationTestCase "Member 1" "(member 3 '(1 2 3 4))" [ExprBool True]
   , integrationTestCase "Member 2" "(member 3 '(1 2 4))" [ExprBool False]
   , integrationTestCase "Map 1" "(map (fn (x) (+ x 2)) '(1 3 2 4))" [ExprList $ ExprInt <$> [3, 5, 4, 6]]
   , integrationTestCase "Map 2" "(map (fn (x) (+ x 2)) '())" [ExprList []]
   , integrationTestCase "Map 3" "(def a 2) (def b 3) (map (fn (x) x) (list 'a 'b))" [ExprAtom "a", ExprAtom "b", ExprList [ExprAtom "a", ExprAtom "b"]]
   , integrationTestCase "Filter 1" "(filter (fn (x) (eq? (mod x 2) 0)) '(1 2 3 4 6 7))" [ExprList $ ExprInt <$> [2, 4, 6]]
   , integrationTestCase "Filter 2" "(filter (fn (x) (eq? (mod x 2) 0)) '())" [ExprList []]
   , integrationTestCaseFail "Filter 3" "(def a #t) (def b #f) (filter (fn (x) x) (list 'a 'b))"
   , integrationTestCase "Find 1" "(find (fn (x) (eq? (mod x 2) 0)) '(1 3 2 4))" [ExprInt 2]
   , integrationTestCase "Find 2" "(find (fn (x) (eq? (mod x 2) 0)) '(6 3 2 4))" [ExprInt 6]
   , integrationTestCase "Find 3" "(find (fn (x) (eq? (mod x 2) 0)) '(1 3 7 6))" [ExprInt 6]
   , integrationTestCase "Find 4" "(find (fn (x) (eq? (mod x 2) 0)) '(1 3 7 9))" [ExprNil]

   ]

  , testGroup "Other functions" [
     integrationTestCase "apply 1" "(apply + '(1 2 3 4))" [ExprInt 10]]]

integrationTestCase :: String -> String -> [ExprI] -> TestTree
integrationTestCase testName expression expectedResults = testCase testName $
  case parserResult of
    Left parseError -> assertFailure "Parser error"
    Right parsed ->
      case runAllParsed parsed coreEnvironment of
        Left interpreterError -> assertFailure interpreterError
        Right (results, newState) -> assertEqual "Different results" (map show expectedResults) (map show results)
  where
    parserResult = parseManyExprWithLines expression

integrationTestCaseFail :: String -> String -> TestTree
integrationTestCaseFail testName expression = testCase testName $
  case parserResult of
    Left parseError -> assertFailure "Parser error"
    Right parsed ->
      case runAllParsed parsed coreEnvironment of
        Left interpreterError -> pure ()
        Right result -> assertFailure "Different results"
  where
    parserResult = parseManyExprWithLines expression
