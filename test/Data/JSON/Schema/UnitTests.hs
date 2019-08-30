{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.JSON.Schema.UnitTests where

import qualified Data.HashMap.Strict as Map
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.QQ.Simple as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Vector          as V
import qualified Test.Tasty           as T
import           Test.Tasty.HUnit     ((@=?))
import qualified Test.Tasty.HUnit     as T.H
import qualified Data.Set as Set

import qualified Data.JSON.Schema     as Sc
import qualified Data.JSON.Validation as Val

tests :: T.TestTree
tests = T.testGroup "unit tests"
  [ randomTest
  , parsingTests
  ]


parsingTests :: T.TestTree
parsingTests = T.testGroup "parsing"
  [ T.H.testCase "parse type validator" $ do
      let raw = [JSON.aesonQQ|
      {"type": "integer"}
      |]
      expectValidators [Sc.ValType $ Sc.OneType Sc.PTInteger] (parseSchema raw)

  , T.H.testCase "parse object validator" $ do
      -- regular expression have an internal pointer address, and that
      -- mess up the Eq instance, so it's not convenient (if possible at all)
      -- to test parsing of patternProperties :/
      let raw = [JSON.aesonQQ|
      { "properties": { "foo": {"type": "integer"} },
        "additionalProperties": {"type": "integer"}
      }
      |]

      let typeValidator = Sc.ValType $ Sc.OneType Sc.PTInteger
      let integSchema = Sc.Schema Nothing Nothing $ V.singleton typeValidator
      let expected = Sc.ValObject $ Sc.ObjectValidator
            { Sc.ovProperties      = Map.singleton "foo" integSchema
            , Sc.ovAdditionalProps = Sc.SomeAdditionalProperties integSchema
            , Sc.ovPatternProps    = mempty
            }

      expectValidators [expected] (parseSchema raw)

  , T.H.testCase "kitchen sink" $ do
      let raw = [JSON.aesonQQ|
      { "properties": {
          "foo": {"type": "integer"},
          "bar": {"type": "array", "minItems": 2, "maxItems": 5},
          "baz": true
        },
        "additionalProperties": false
      }
      |]

      let typeValidator = Sc.ValType $ Sc.OneType Sc.PTInteger
      let arrValidator = Sc.ArrayValidator (Just 2) (Just 5)
      let fooSchema = Sc.Schema Nothing Nothing $ V.singleton typeValidator
      let barSchema = Sc.Schema Nothing Nothing $ V.fromList
            [ Sc.ValType (Sc.OneType Sc.PTArray)
            , Sc.ValArray arrValidator
            ]
      let bazSchema = Sc.Schema Nothing Nothing $ V.singleton (Sc.ValBool True)
      let expected = Sc.ValObject $ Sc.ObjectValidator
            { Sc.ovProperties      = Map.fromList
                [ ("foo", fooSchema)
                , ("bar", barSchema)
                , ("baz", bazSchema)
                ]
            , Sc.ovAdditionalProps = Sc.NoAdditionalProperties
            , Sc.ovPatternProps    = mempty
            }

      expectValidators [expected] (parseSchema raw)

  ]


randomTest :: T.TestTree
randomTest = T.H.testCaseSteps "boom" $ \step -> do
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
  step ("parsed schema: " <> show schema)
  -- T.H.assertEqual "nope" (Val.Ok val) result
  pure ()

parseSchema :: JSON.Value -> Either String Sc.JSONSchema
parseSchema = JSON.parseEither JSON.parseJSON

-- in any order
expectValidators :: [Sc.Validator] -> Either String Sc.JSONSchema -> T.H.Assertion
expectValidators expectedValidators = \case
  Left err -> T.H.assertFailure $ "Expected a json schema but got a parse error: " <> show err
  Right schema -> Set.fromList expectedValidators @=? Set.fromList (V.toList $ Sc.validators schema)
