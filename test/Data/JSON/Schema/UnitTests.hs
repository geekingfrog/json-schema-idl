{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.JSON.Schema.UnitTests where

import qualified Data.Text as Tx
import qualified Data.HashMap.Strict as Map
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.QQ.Simple as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Vector          as V
import qualified Test.Tasty           as T
import           Test.Tasty.HUnit     ((@=?))
import qualified Test.Tasty.HUnit     as T.H
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

import qualified Data.JSON.Schema     as Sc
import qualified Data.JSON.Validation as Val

tests :: T.TestTree
tests = T.testGroup "unit tests"
  [ randomTest ]


-- parsingTests :: T.TestTree
-- parsingTests = T.testGroup "parsing"
--   [ T.H.testCase "parse type validator" $ do
--       let raw = [JSON.aesonQQ|
--       {"type": "integer"}
--       |]
--       let expected = Sc.ValAny $ Sc.AnyValidator
--             { Sc.anyType = Just (Sc.OneType Sc.PTInteger)
--             -- , anyEnum :: EnumValidator
--             , Sc.anyConst = Nothing
--             }
--       expectValidators [expected] (parseSchema raw)
--
--   , T.H.testCase "parse object validator" $ do
--       -- regular expression have an internal pointer address, and that
--       -- mess up the Eq instance, so it's not convenient (if possible at all)
--       -- to test parsing of patternProperties :/
--       let raw = [JSON.aesonQQ|
--       { "properties": { "foo": {"type": "integer"} },
--         "additionalProperties": {"type": "integer"}
--       }
--       |]
--
--       let anyValidator = Sc.ValAny $ Sc.AnyValidator
--             { Sc.anyType = Just (Sc.OneType Sc.PTInteger)
--             -- , anyEnum :: EnumValidator
--             , Sc.anyConst = Nothing
--             }
--       let integSchema = Sc.Schema Nothing Nothing $ V.singleton anyValidator
--       let expected = Sc.ValObject $ Sc.ObjectValidator
--             { Sc.ovProperties      = Map.singleton "foo" integSchema
--             , Sc.ovAdditionalProps = Sc.SomeAdditionalProperties integSchema
--             , Sc.ovPatternProps    = mempty
--             }
--
--       expectValidators [expected] (parseSchema raw)
--
--   , T.H.testCase "kitchen sink" $ do
--       let raw = [JSON.aesonQQ|
--       { "properties": {
--           "foo": {"type": "integer"},
--           "bar": {"type": "array", "minItems": 2, "maxItems": 5},
--           "baz": true
--         },
--         "additionalProperties": false
--       }
--       |]
--
--       let anyValidator = Sc.ValAny $ Sc.AnyValidator
--             { Sc.anyType = Just (Sc.OneType Sc.PTInteger)
--             -- , anyEnum :: EnumValidator
--             , Sc.anyConst = Nothing
--             }
--       let arrValidator = Sc.ArrayValidator
--             { Sc.avMinItems        = Just 2
--             , Sc.avMaxItems        = Just 5
--             , Sc.avItems           = Sc.NoItemsValidator
--             , Sc.avAdditionalItems = Sc.AdditionalAllAllowed
--             , Sc.avUniqueItems     = Sc.ItemsCanBeDuplicated
--             , Sc.avContainsItem    = Nothing
--             }
--       let fooSchema = Sc.Schema Nothing Nothing $ V.singleton anyValidator
--       let barSchema = Sc.Schema Nothing Nothing $ V.fromList []
--             -- [ Sc.ValType (Sc.OneType Sc.PTArray)
--             -- , Sc.ValArray arrValidator
--             -- ]
--       let bazSchema = Sc.Schema Nothing Nothing $ V.singleton (Sc.ValBool True)
--       let expected = Sc.ValObject $ Sc.ObjectValidator
--             { Sc.ovProperties      = Map.fromList
--                 [ ("foo", fooSchema)
--                 , ("bar", barSchema)
--                 , ("baz", bazSchema)
--                 ]
--             , Sc.ovAdditionalProps = Sc.NoAdditionalProperties
--             , Sc.ovPatternProps    = mempty
--             }
--
--       -- print ("kitchen sink, parsed schema: " <> show parseSchema raw)
--       expectValidators [expected] (parseSchema raw)
--
--   ]


randomTest :: T.TestTree
randomTest = T.H.testCaseSteps "boom" $ \step -> do
  let val = [JSON.aesonQQ|{"fxo": []}|]
  let rawSchema = [JSON.aesonQQ|
      {
          "properties": {
              "foo": {"type": "array", "maxItems": 3},
              "bar": {"type": "array"}
          },
          "patternProperties": {"f.o": {"minItems": 2}},
          "additionalProperties": {"type": "integer"}
      }
    |]

  case parseSchema rawSchema of
    Left err -> do
      step err
      T.H.assertFailure "schema failed to parse"
    Right schema -> do
      step $ "parsed schema: " <> show schema
      let result = Val.runValidation (Val.validate schema val) Val.emptyValidationEnv
      step $ Tx.unpack $ "result of validation: " <> Val.prettyValidationOutcome result
      case result of
        Val.Error e -> step $ "number of errors: " <> show (length e)
        _ -> pure ()
      pure ()

  -- let result = Val.validate schema val
  -- step ("parsed schema: " <> show schema)
  -- T.H.assertEqual "nope" (Val.Ok val) result

parseSchema :: JSON.Value -> Either String Sc.JSONSchema
parseSchema = JSON.parseEither JSON.parseJSON

-- -- in any order
-- expectValidators :: (HasCallStack) => [Sc.Validator] -> Either String Sc.JSONSchema -> T.H.Assertion
-- expectValidators expectedValidators = \case
--   Left err -> T.H.assertFailure $ "Expected a json schema but got a parse error: " <> show err
--   Right schema -> Set.fromList expectedValidators @=? Set.fromList (V.toList $ Sc.validators schema)
