{-# LANGUAGE QuasiQuotes #-}

module Data.JSON.Schema.UnitTests where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T.H
import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Aeson.QQ.Simple    as JSON

import qualified Data.JSON.Schema as Sc
import qualified Data.JSON.Validation as Val

tests :: T.TestTree
tests = T.testGroup "unit tests"
  [ T.H.testCase "boom" $ do
      let val = [JSON.aesonQQ|{"foo": [1, 2, 3, 4]}|]
      let (Right schema) = JSON.parseEither JSON.parseJSON [JSON.aesonQQ|
        {
            "properties": {
                "foo": {"type": "array", "maxItems": 3},
                "bar": {"type": "array"}
            },
            "patternProperties": {"f.o": {"minItems": 2}},
            "additionalProperties": {"type": "integer"}
        }
        |]
      let result = Val.validate schema val
      print schema
      T.H.assertEqual "nope" (Val.Ok val) result
  ]
