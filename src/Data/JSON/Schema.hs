{-# LANGUAGE RecordWildCards #-}
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
import Data.Scientific

data JSONSchema
  = RootSchema RootMetadata Schema
  | SubSchema Schema
  deriving (Eq, Show)

instance JSON.FromJSON JSONSchema where
  parseJSON raw
    = JSON.withObject "object JSONschema" parseObject raw
    <|> JSON.withBool "boolean JSONschema" parseBool raw

    where
      parseObject o = do
        mbVersion <- o .:? "$schema" -- TODO check this is a valid URI
        i <- o .:? "$id"
        case mbVersion of
          Just ver -> RootSchema (RootMetadata ver i) <$> JSON.parseJSON raw
          Nothing -> SubSchema <$> JSON.parseJSON raw
      parseBool b = pure $ SubSchema $ Schema Nothing Nothing (V.singleton (ValBool b))


schema :: JSONSchema -> Schema
schema = \case
  (RootSchema _ s) -> s
  (SubSchema s) -> s

validators :: JSONSchema -> V.Vector Validator
validators = \case
  (RootSchema _ s) -> sValidators s
  (SubSchema s) -> sValidators s

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
  , sValidators :: V.Vector Validator
  }
  deriving (Eq, Show, Ord, Generic)

instance JSON.FromJSON Schema where
  parseJSON raw
    = JSON.withObject "object subschema" parseObject raw
    <|> JSON.withBool "boolean subschema" parseBool raw

    where
      parseObject o = do
        desc <- o .:? "description"
        title <- o .:? "title"
        validations <- parseAllValidators o
        pure $ Schema desc title validations

      parseBool b = pure $ Schema Nothing Nothing (V.singleton (ValBool b))

data Validator
  = ValType TypeValidator
  | ValObject ObjectValidator
  | ValBool Bool
  | ValArray ArrayValidator
  | ValNumeric NumericValidator
  deriving (Eq, Show, Ord)

parseAllValidators :: JSON.Object -> JSON.Parser (V.Vector Validator)
parseAllValidators o = do
  vals <- traverse optional
    [ ValType <$> parseTypeValidator o
    , ValObject <$> parseObjectValidator o
    , ValArray <$> parseArrayValidator o
    , ValNumeric <$> parseNumericValidator o
    ]
  pure $ V.fromList $ Mb.catMaybes vals


data TypeValidator
  = OneType PrimitiveType
  | MultipleTypes (V.Vector PrimitiveType)
  deriving (Eq, Show, Ord)

data PrimitiveType
  = PTNull
  | PTBoolean
  | PTObject
  | PTArray
  | PTNumber
  | PTInteger
  | PTString
  deriving (Eq, Show, Ord, Generic)

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

parseTypeValidator :: JSON.Object -> JSON.Parser TypeValidator
parseTypeValidator o = o .:? "type" >>= \case
  Nothing -> fail "No `type` key found"
  Just typVal -> parseOneType typVal <|> parseMultipleTypes typVal

    where
      parseOneType :: JSON.Value -> JSON.Parser TypeValidator
      parseOneType = fmap OneType . JSON.parseJSON

      parseMultipleTypes :: JSON.Value -> JSON.Parser TypeValidator
      parseMultipleTypes = fmap MultipleTypes . JSON.parseJSON


data ObjectValidator = ObjectValidator
  { ovProperties      :: Map.HashMap Text Schema
  , ovAdditionalProps :: AdditionalProperties
  , ovPatternProps    :: OrdMap.Map RE.Regex Schema
  }
  deriving (Eq, Show, Ord)

data AdditionalProperties
  = NoAdditionalProperties
  | SomeAdditionalProperties Schema
  | AllAdditionalProperties
  deriving (Eq, Show, Ord)

parseObjectValidator :: JSON.Object -> JSON.Parser ObjectValidator
parseObjectValidator o = do
  props <- o .:? "properties" .!= mempty
  ap <- parseAdditionalProperties o
  patterns <- parsePatternProperties o
  if ap == AllAdditionalProperties && Map.null props && OrdMap.null patterns
    then fail "no validation properties present"
    else pure $ ObjectValidator
           { ovProperties = props
           , ovAdditionalProps = ap
           , ovPatternProps = patterns
           }

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


data ArrayValidator = ArrayValidator
  { avMinItems        :: Maybe Int
  , avMaxItems        :: Maybe Int
  , avItems           :: ItemsValidator
  , avAdditionalItems :: AdditionalItemsValidator
  }
  deriving (Eq, Show, Ord)

data ItemsValidator
  = SingleSchema Schema
  | MultipleSchemas (V.Vector Schema)
  | NoItemsValidator
  deriving (Eq, Show, Ord)

data AdditionalItemsValidator
  = AdditionalSingleSchema Schema
  | AdditionalMultipleSchemas (V.Vector Schema)
  | AdditionalAllAllowed
  | AdditionalAllForbidden
  deriving (Eq, Show, Ord)

parseArrayValidator :: JSON.Object -> JSON.Parser ArrayValidator
parseArrayValidator o = do
  avMinItems <- o .:? "minItems"
  avMaxItems <- o .:? "maxItems"
  avItems <- parseItemsValidator (Map.lookup "items" o)
  avAdditionalItems <- parseAdditionalItemsValidator (Map.lookup "additionalItems" o)

  case (avMinItems, avMaxItems, avItems, avAdditionalItems) of
    (Nothing, Nothing, NoItemsValidator, AdditionalAllAllowed) -> fail "no array properties to validate"
    _ -> pure $ ArrayValidator{..}

  where
    parseItemsValidator = \case
      Nothing -> pure NoItemsValidator
      Just jsonValue ->
        (SingleSchema <$> JSON.parseJSON jsonValue)
        <|> (MultipleSchemas <$> JSON.parseJSON jsonValue)

    parseAdditionalItemsValidator = \case
      Nothing -> pure AdditionalAllAllowed
      Just jsonValue ->
        (AdditionalSingleSchema <$> JSON.parseJSON jsonValue)
        <|> (AdditionalMultipleSchemas <$> JSON.parseJSON jsonValue)
        <|> parseBool jsonValue

    parseBool = \case
      JSON.Bool b -> if b
        then pure AdditionalAllAllowed
        else pure AdditionalAllForbidden
      _ -> fail "not a boolean for additionalItems keyword"

data NumericValidator = NumericValidator
  { nvMultipleOf       :: Maybe Scientific
  , nvMinimum          :: Maybe Scientific
  , nvMaximum          :: Maybe Scientific
  , nvExclusiveMinimum :: Maybe Scientific
  , nvExclusiveMaximum :: Maybe Scientific
  }
  deriving (Eq, Show, Ord)

parseNumericValidator :: JSON.Object -> JSON.Parser NumericValidator
parseNumericValidator o = do
  nvMultipleOf       <- o .:? "multipleOf"
  nvMinimum          <- o .:? "minimum"
  nvMaximum          <- o .:? "maximum"
  nvExclusiveMinimum <- o .:? "exclusiveMinimum"
  nvExclusiveMaximum <- o .:? "exclusiveMaximum"
  case nvMultipleOf <|> nvMinimum <|> nvMaximum <|> nvExclusiveMinimum <|> nvExclusiveMaximum of
    Nothing -> fail "no numeric validator"
    Just _ -> pure NumericValidator{..}
