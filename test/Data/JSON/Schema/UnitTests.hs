{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.JSON.Schema.UnitTests where

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.QQ.Simple as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.HashMap.Strict  as Map
import qualified Data.Map.Strict      as OrdMap
import qualified Data.Set             as Set
import qualified Data.Text            as Tx
import qualified Data.Vector          as V
import           GHC.Stack            (HasCallStack)
import qualified Test.Tasty           as T
import           Test.Tasty.HUnit     ((@=?))
import qualified Test.Tasty.HUnit     as T.H

import qualified Data.JSON.Schema     as Sc
import qualified Data.JSON.Validation as Val

tests :: T.TestTree
tests = T.testGroup "unit tests"
  [ randomTest
  , aggregateReferences
  , miscTest
  ]


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

aggregateReferences :: T.TestTree
aggregateReferences = T.H.testCase "aggregateReferences" $ do
  schema <- JSON.eitherDecodeFileStrict' "./test/def.json" >>= \case
    Left err -> T.H.assertFailure err
    Right s -> pure s
  let refs = Sc.aggregateReferences schema
  let expected = Set.fromList
        [ "http://example.com/other.json"
        , "http://example.com/other.json#/definitions/X"
        , "http://example.com/other.json#/definitions/Y"
        , "http://example.com/other.json#bar"
        , "http://example.com/root.json"
        , "http://example.com/root.json#/definitions/A"
        , "http://example.com/root.json#/definitions/B"
        , "http://example.com/root.json#/definitions/B/definitions/X"
        , "http://example.com/root.json#/definitions/B/definitions/Y"
        , "http://example.com/root.json#/definitions/C"
        , "http://example.com/root.json#foo"
        , "http://example.com/t/inner.json"
        , "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"
        ]
  Set.fromList (show . Sc.getURI <$> OrdMap.keys refs) @=? expected


randomTest :: T.TestTree
randomTest = T.H.testCaseSteps "boom" $ \step -> do
  let rawSchema = [JSON.aesonQQ|
        {
            "properties": {
                "foo": {"$ref": "#"}
            },
            "additionalProperties": false
        }
      |]
  let val = [JSON.aesonQQ|{"foo": {"foo": false}}|]

  case parseSchema rawSchema of
    Left err -> do
      step err
      T.H.assertFailure "schema failed to parse"
    Right schema -> do
      step $ "parsed schema: " <> show schema
      let result = Val.validateSchema schema val
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

miscTest :: T.TestTree
miscTest = T.H.testCaseSteps "misc" $ \step -> do
  sanitize "#" @=? ""
  sanitize "#/" @=? ""

sanitize :: String -> String
sanitize = \case
  [] -> []
  ('/' : rest) -> rest
  ('#' : rest) -> sanitize rest
  frag -> frag


-- -- in any order
-- expectValidators :: (HasCallStack) => [Sc.Validator] -> Either String Sc.JSONSchema -> T.H.Assertion
-- expectValidators expectedValidators = \case
--   Left err -> T.H.assertFailure $ "Expected a json schema but got a parse error: " <> show err
--   Right schema -> Set.fromList expectedValidators @=? Set.fromList (V.toList $ Sc.validators schema)
