{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSON.Schema.TestSuite where

import qualified Control.Monad.Reader as Rdr
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Char            as Chr
import qualified Data.Foldable        as F
import           Data.Text            (Text)
import qualified Data.Text            as Tx
import qualified Data.Vector          as V
import           GHC.Generics
import qualified Test.Tasty           as T
import qualified Test.Tasty.HUnit     as T.H

import qualified Data.JSON.Schema     as Sc
import qualified Data.JSON.Validation as Val
import qualified Data.JSON.Draft07    as Draft07

data RawTestSuite = RawTestSuite
  { rtsDescription :: Text
  , rtsSchema :: JSON.Value
  , rtsTests :: V.Vector TestSpec
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RawTestSuite where
  parseJSON = parseJSONHungarian

data TestSuite = TestSuite
  { tsDescription :: Text
  , tsSchema :: Sc.JSONSchema
  , tsTests :: V.Vector TestSpec
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON TestSuite where
  parseJSON = parseJSONHungarian

data TestSpec = TestSpec
  { specDescription :: Text
  , specData :: JSON.Value
  , specValid :: Bool
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON TestSpec where
  parseJSON = parseJSONHungarian

parseJSONHungarian
  :: ( Generic a
     , JSON.GFromJSON JSON.Zero (Rep a)
     )
  => JSON.Value
  -> JSON.Parser a

parseJSONHungarian = JSON.genericParseJSON (JSON.defaultOptions
  { JSON.fieldLabelModifier = dropHungarian
  })

  where
    dropHungarian :: String -> String
    dropHungarian = \case
      [] -> []
      (x:xs) -> if Chr.isLower x
        then dropHungarian xs
        else Chr.toLower x : xs


parseSchema :: RawTestSuite -> Either String TestSuite
parseSchema rts = case JSON.parseEither JSON.parseJSON (rtsSchema rts) of
  Left err -> Left $ "Cannot parse JSON schema for '" <> Tx.unpack (rtsDescription rts) <> "': " <> err
  Right s -> pure (TestSuite (rtsDescription rts) s (rtsTests rts))


buildOfficialTestSuites :: IO T.TestTree
buildOfficialTestSuites = do
  let prefix = "./JSON-Schema-Test-Suite/tests/draft7/"
  tests <- traverse (buildTestSuite prefix)
    -- [ "additionalItems.json"
    -- , "additionalProperties.json"
    -- , "allOf.json"
    -- , "anyOf.json"
    -- , "boolean_schema.json"
    -- , "const.json"
    -- , "contains.json"
    -- -- , "default.json" -- not sure what this is, not in the v7 spec as validator for object
    -- , "definitions.json"
    -- , "dependencies.json"
    -- , "enum.json"
    -- , "exclusiveMaximum.json"
    -- , "exclusiveMinimum.json"
    -- , "if-then-else.json"
    -- , "items.json" -- requires definitions, references to def and additionalItems to fully pass
    -- , "maximum.json"
    -- , "maxItems.json"
    -- , "maxLength.json"
    -- , "maxProperties.json"
    -- , "minimum.json"
    -- , "minItems.json"
    -- , "minLength.json"
    -- , "minProperties.json"
    -- , "multipleOf.json"
    -- , "not.json"
    -- , "oneOf.json"
    -- , "pattern.json"
    -- , "patternProperties.json"
    -- , "properties.json"
    -- , "propertyNames.json"
    -- , "ref.json"
    -- -- , "refRemote.json"
    -- , "required.json"
    -- , "type.json"
    -- , "uniqueItems.json"
    -- ]
    ["refRemote.json"]

  pure $ T.testGroup "JSON schema official test suite" tests

buildCustomTestSuites :: IO T.TestTree
buildCustomTestSuites = do
  tests <- buildTestSuite "./test/resources/" "custom.json"
  pure $ T.testGroup "Custom schema tests" [tests]

buildTestSuite :: FilePath -> FilePath -> IO T.TestTree
buildTestSuite prefix name = do
  suites <- loadTestSuite (prefix <> name)
  pure $ T.testGroup name $ F.toList (fmap mkTestGroup suites)

mkTestGroup :: TestSuite -> T.TestTree
mkTestGroup ts = T.testGroup (Tx.unpack $ tsDescription ts) $
  F.toList (fmap (mkTestCase (tsSchema ts)) (tsTests ts))

mkTestCase :: Sc.JSONSchema -> TestSpec -> T.TestTree
mkTestCase schema spec = T.H.testCase (Tx.unpack $ specDescription spec) $ do
  let env = case Val.addSchema (Val.loadSchema schema) Draft07.schema of
        Left err -> error (Tx.unpack err)
        Right x -> x

  case (specValid spec, Val.validateSchema env (specData spec)) of
    (True, Val.Error errs) ->
      T.H.assertFailure $ "Expected to be valid but failed with: " <> show errs <> " - for value: " <> show (specData spec) <> " - and schema: " <> show schema
    (False, Val.Ok _) ->
      T.H.assertFailure "Expected to fail validation but succeeded"
    _ -> pure ()

loadTestSuite :: String -> IO (V.Vector TestSuite)
loadTestSuite fullPath =
  JSON.eitherDecodeFileStrict fullPath >>= \case
    Left err -> T.H.assertFailure $ "Cannot decode test suite: " <> err
    Right testSuites -> case traverse parseSchema testSuites of
      Left err -> T.H.assertFailure $ "Cannot decode json schema: " <> err
      Right x -> pure x
