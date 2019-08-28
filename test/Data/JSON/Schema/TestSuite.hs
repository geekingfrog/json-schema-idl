{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSON.Schema.TestSuite where

import qualified Data.Foldable as F
import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Char        as Chr
import           Data.Text        (Text)
import qualified Data.Text        as Tx
import qualified Data.Vector      as V
import           GHC.Generics
import qualified Test.Tasty       as T
import qualified Test.Tasty.HUnit as T.H

import qualified Data.JSON.Schema as Sc
import qualified Data.JSON.Validation as Val

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


buildTestSuites :: IO T.TestTree
buildTestSuites = do
  tests <- traverse buildTestSuite ["type.json"]
  pure $ T.testGroup "JSON schema official test suite" tests

buildTestSuite :: String -> IO T.TestTree
buildTestSuite filePath = do
  suites <- loadTestSuite filePath
  pure $ T.testGroup filePath $ F.toList (fmap mkTestGroup suites)

mkTestGroup :: TestSuite -> T.TestTree
mkTestGroup ts = T.testGroup (Tx.unpack $ tsDescription ts) $
  F.toList (fmap (mkTestCase (tsSchema ts)) (tsTests ts))

mkTestCase :: Sc.JSONSchema -> TestSpec -> T.TestTree
mkTestCase schema spec = T.H.testCase (Tx.unpack $ specDescription spec) $
  case (specValid spec, Val.validate schema (specData spec)) of
    (True, Val.Error errs) ->
      T.H.assertFailure $ "Expected to be valid but failed with: " <> show errs <> " - for value: " <> show (specData spec)
    (False, Val.Ok _) ->
      T.H.assertFailure "Expected to fail validation but succeeded"
    _ -> pure ()

loadTestSuite :: String -> IO (V.Vector TestSuite)
loadTestSuite filePath =
  let fullPath = "./JSON-Schema-Test-Suite/tests/draft7/" <> filePath
   in JSON.eitherDecodeFileStrict fullPath >>= \case
        Left err -> T.H.assertFailure $ "Cannot decode test suite: " <> err
        Right testSuites -> case traverse parseSchema testSuites of
          Left err -> T.H.assertFailure $ "Cannot decode json schema: " <> err
          Right x -> pure x