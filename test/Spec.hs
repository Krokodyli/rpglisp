module Main where

import Test.Tasty
import Test.Tasty.HUnit
import RpgLisp.Grammar
import RpgLisp.Test.Parser
import RpgLisp.Test.Integration

main :: IO ()
main = defaultMain $ testGroup "Tests" [
    parserTests
  , integrationTests
  ]
