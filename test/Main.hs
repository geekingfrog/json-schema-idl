module Main where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T.H

import qualified Data.JSON.Schema.TestSuite as TS

main :: IO ()
main = TS.buildTestSuites >>= T.defaultMain
