module RpgLisp.Test.Parser where

import RpgLisp.Grammar
import RpgLisp.Parser
import Data.Either
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: TestTree
parserTests = testGroup "Parser"
  [
      parserTestCase "1" (PExprInt 1)
    , parserTestCase "-1" (PExprInt $ -1)
    , parserTestCase "123456789" (PExprInt 123456789)
    , parserTestCase "\"\"" (PExprString "")
    , parserTestCase "\"strinG\n\"" (PExprString "strinG\n")
    , parserTestCase "#f" (PExprBool False)
    , parserTestCase "#t" (PExprBool True)
    , parserTestCase "#nil" PExprNil
    , parserTestCase "atom" (PExprAtom "atom")
    , parserTestCase "'atom" (PExprList [PExprAtom "quote", PExprAtom "atom"])
    , parserTestCase "(1 2 3)" (PExprList $ PExprInt <$> [1, 2, 3])
    , parserTestCase "(\"s\" \"t\" \"r\")" (PExprList $ PExprString <$> ["s", "t", "r"])
    , parserTestCase "()" (PExprList [])
    , parserTestCase "(#nil #f)" (PExprList [PExprNil, PExprBool False])
    , parserTestCase "1 ;; comment" (PExprInt 1)
    , parserTestCase "(+ 1 2)" (PExprList [PExprAtom "+", PExprInt 1, PExprInt 2])
    , parserTestCase "(-func-)" (PExprList [PExprAtom "-func-"])
    , parserTestCase "(f)" (PExprList [PExprAtom "f"])
    , parserTestCase "(1 (2 3 4) ())" (PExprList [PExprInt 1, PExprList $ PExprInt <$> [2, 3, 4], PExprList []])
    , parserTestCase "'(1 (2 3) 4)" (PExprList [PExprAtom "quote", PExprList [PExprInt 1, PExprList [PExprInt 2, PExprInt 3], PExprInt 4]])
    , QC.testProperty "Parser QuickCheck tests" parserProp
  ]

parserTestCase :: String -> ParsedExpr -> TestTree
parserTestCase strExpr parsedExpr = testCase strExpr
  $ parseSingleExpr strExpr @?= Right parsedExpr

parserTestCaseFail :: String -> TestTree
parserTestCaseFail strExpr = testCase strExpr $ assertBool strExpr $ isRight (parseSingleExpr strExpr)

showParseExpr :: ParsedExpr -> String
showParseExpr (PExprInt val) = show val
showParseExpr (PExprString val) = show val
showParseExpr (PExprBool val) = if val then "#t" else "#f"
showParseExpr PExprNil = "#nil"
showParseExpr (PExprAtom val) = val
showParseExpr (PExprList val) = "(" ++ unwords (map showParseExpr val) ++ ")"

generateList :: QC.Gen ParsedExpr
generateList = QC.chooseInt (1 , 10) >>=
  \n -> PExprList <$> QC.vectorOf n generateListElem

generateWord :: QC.Gen String
generateWord = QC.listOf1 $ QC.elements $ concat [
  ['a'..'z'],
  ['A'..'Z'],
  ['-', '+', '*', '/', '@', '?', '!']]

generateListElem :: QC.Gen ParsedExpr
generateListElem = QC.oneof [
      PExprInt <$> QC.arbitrary,
      PExprString <$> generateWord,
      PExprBool <$> QC.arbitrary,
      pure PExprNil,
      PExprAtom <$> generateWord,
      generateList]

newtype ParserInputString = ParserInputString String

instance QC.Arbitrary ParsedExpr where
  arbitrary = generateList

parserProp :: ParsedExpr -> Bool
parserProp parsedExpr = parseSingleExpr (showParseExpr parsedExpr) == Right parsedExpr
