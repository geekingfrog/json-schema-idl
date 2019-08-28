{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.JSON.Schema where

import           Control.Applicative
import           Data.Aeson            ((.!=), (.:), (.:?))
import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Types      as JSON
import qualified Data.Char             as Chr
import qualified Data.Foldable         as F
import qualified Data.HashMap.Strict   as Map
import qualified Data.Map.Strict       as OrdMap
import qualified Data.Maybe            as Mb
import           Data.Text             (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text             as Tx
import qualified Data.Vector           as V
import           GHC.Generics
import qualified Text.Regex.PCRE.Heavy as RE

data JSONSchema
  = RootSchema RootMetadata Schema
  | SubSchema Schema
  deriving (Eq, Show)

instance JSON.FromJSON JSONSchema where
  parseJSON raw = flip (JSON.withObject "json schema") raw $ \o -> do
    mbVersion <- o .:? "$schema" -- TODO check this is a valid URI
    i <- o .:? "$id"
    case mbVersion of
      Just ver -> RootSchema (RootMetadata ver i) <$> JSON.parseJSON raw
      Nothing -> SubSchema <$> JSON.parseJSON raw
    -- error "wip fromJSON JSONSchema"


schema :: JSONSchema -> Schema
schema = \case
  (RootSchema _ s) -> s
  (SubSchema s) -> s

data RootMetadata = RootMetadata
  { rmVersion :: SchemaVersion
  , rmId :: Maybe Text
  }
  deriving (Eq, Show)

data SchemaVersion = Draft07
  deriving (Eq, Show)

instance JSON.FromJSON SchemaVersion where
  parseJSON = JSON.withText "$schema" $ \t ->
    if t == "http://json-schema.org/draft-07/schema#"
      then pure Draft07
      else fail $ "Only draft 7 is supported, got $schema = " <> Tx.unpack t

data Schema = Schema
  { sDescription :: Maybe Text
  , sTitle :: Maybe Text
  , sValidations :: V.Vector Validation
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON Schema where
  parseJSON raw
    = JSON.withObject "object schema" parseObject raw
    <|> JSON.withBool "boolean schema" parseBool raw

    where
      parseObject o = do
        desc <- o .:? "description"
        title <- o .:? "title"
        validations <- parseValidationsObject o
        pure $ Schema desc title validations

      parseBool b = pure $ Schema Nothing Nothing (V.singleton (ValBool b))


data Validation
  = ValType ValidationType
  | ValProperties ValidationProperties
  | ValBool Bool
  deriving (Eq, Show)

parseValidationsObject :: JSON.Object -> JSON.Parser (V.Vector Validation)
parseValidationsObject o = do
  vals <- traverse optional
    [ ValType <$> parseValidationType o
    , ValProperties <$> parseValidationProperties o
    ]
  pure $ V.fromList $ Mb.catMaybes vals


data ValidationType
  = OneType PrimitiveType
  | MultipleTypes (V.Vector PrimitiveType)
  deriving (Show, Eq)

data PrimitiveType
  = PTNull
  | PTBoolean
  | PTObject
  | PTArray
  | PTNumber
  | PTInteger
  | PTString
  deriving (Show, Eq, Generic)

prettyPrimitiveType :: PrimitiveType -> Text
prettyPrimitiveType = \case
  PTNull    -> "null"
  PTBoolean -> "boolean"
  PTObject  -> "object"
  PTArray   -> "array"
  PTNumber  -> "number"
  PTInteger -> "integer"
  PTString  -> "string"

instance JSON.ToJSON PrimitiveType where
  toJSON = JSON.genericToJSON $ JSON.defaultOptions
    { JSON.constructorTagModifier = (\(x:xs) -> Chr.toLower x : xs) . drop 2
    }

instance JSON.FromJSON PrimitiveType where
  parseJSON = JSON.genericParseJSON $ JSON.defaultOptions
    { JSON.constructorTagModifier = (\(x:xs) -> Chr.toLower x : xs) . drop 2
    }

parseValidationType :: JSON.Object -> JSON.Parser ValidationType
parseValidationType o = o .:? "type" >>= \case
  Nothing -> fail "No `type` key found"
  Just typVal -> parseOneType typVal <|> parseMultipleTypes typVal

    where
      parseOneType :: JSON.Value -> JSON.Parser ValidationType
      parseOneType = fmap OneType . JSON.parseJSON

      parseMultipleTypes :: JSON.Value -> JSON.Parser ValidationType
      parseMultipleTypes = fmap MultipleTypes . JSON.parseJSON


data ValidationProperties = ValidationProperties
  { vpProperties      :: Map.HashMap Text Schema
  , vpAdditionalProps :: AdditionalProperties
  , vpPatternProps    :: OrdMap.Map RE.Regex Schema
  }
  deriving (Eq, Show)

data AdditionalProperties
  = NoAdditionalProperties
  | SomeAdditionalProperties (V.Vector Schema)
  | AllAdditionalProperties
  deriving (Eq, Show)

parseAdditionalProperties :: JSON.Object -> JSON.Parser AdditionalProperties
parseAdditionalProperties o = o .:? "additionalProperties" >>= \case
  Nothing -> pure AllAdditionalProperties
  Just (JSON.Bool True) -> pure AllAdditionalProperties
  Just (JSON.Bool False) -> pure NoAdditionalProperties
  Just x -> SomeAdditionalProperties <$> JSON.parseJSON x

parsePatternProperties :: JSON.Object -> JSON.Parser (OrdMap.Map RE.Regex Schema)
parsePatternProperties o = o .:? "patternProperties" >>= \case
  Nothing -> pure mempty
  Just x -> flip (JSON.withObject "patternProperties object") x $ \o' ->
    case traverse mkTuples (Map.toList o') of
      Left err -> fail err
      Right keyVals -> pure $ OrdMap.fromList keyVals

  where
    pcreOptions = []
    mkTuples :: (Text, JSON.Value) -> Either String (RE.Regex, Schema)
    mkTuples (k, v) = do
      r <- RE.compileM (encodeUtf8 k) pcreOptions
      s <- JSON.parseEither JSON.parseJSON v
      pure (r, s)
    -- case RE.compileM (encodeUtf8 k) pcreOptions of
    --   Left err -> Left err
    --   Right r -> Right (r, v)

parseValidationProperties :: JSON.Object -> JSON.Parser ValidationProperties
parseValidationProperties o = do
  props <- o .:? "properties" .!= mempty
  ap <- parseAdditionalProperties o
  patterns <- parsePatternProperties o
  if ap == AllAdditionalProperties && Map.null props && OrdMap.null patterns
    then fail "no validation properties present"
    else pure $ ValidationProperties
           { vpProperties = props
           , vpAdditionalProps = ap
           , vpPatternProps = patterns
           }
