module Main where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T.H

import qualified Data.JSON.Schema.TestSuite as TS
import qualified Data.JSON.Schema.UnitTests as Unit

main :: IO ()
main = do
  testSuite <- TS.buildOfficialTestSuites
  customTestSuite <- TS.buildCustomTestSuites
  let fullTests = T.testGroup "All Tests"
        -- [ testSuite
        -- , customTestSuite
        -- , Unit.tests
        -- ]
        [Unit.tests]
  T.defaultMain fullTests
