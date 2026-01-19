module Main (main) where

import Test.Tasty
import Moe.Parsing.CollectionSpec qualified as CollectionSpec
import Moe.Washing.CompareSpec qualified as WashingCompare

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Moe Bangumi"
  [ WashingCompare.tests
  , CollectionSpec.tests
  ]
