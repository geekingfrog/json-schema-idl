{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSON.Schema where

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Char as Chr
import           Data.Aeson       ((.:), (.:?))
import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Text        (Text)
import qualified Data.Text        as Tx
import qualified Data.Vector      as V
import GHC.Generics
import qualified Data.HashMap.Strict as Map

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
  parseJSON raw = flip (JSON.withObject "schema") raw $ \o -> do
    desc <- o .:? "description"
    title <- o .:? "title"
    validations <- parseValidations o
    pure $ Schema desc title validations

-- instance JSON.ToJSON Schema where
--   toJSON s =
--     let m = validationToJSON (sValidation s)
--      in JSON.Object $ m
--           <> maybe mempty (Map.singleton "description" . JSON.String) (sDescription s)
--           <> maybe mempty (Map.singleton "title" . JSON.String) (sTitle s)


newtype Validation
  = ValType ValidationType
  deriving (Eq, Show)

parseValidations :: JSON.Object -> JSON.Parser (V.Vector Validation)
parseValidations o = V.singleton . ValType <$> parseValidationType o

-- data ValidationAny = ValidationAny
--   { vaType :: ValAnyType
--   , vaEnum :: V.Vector JSON.Value
--   }
--   deriving (Eq, Show)

-- validationToJSON :: Validation -> JSON.Object
-- validationToJSON v = error "wip validationToJSON"

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
  Nothing -> fail "No `type` property"
  Just typVal -> parseOneType typVal <|> parseMultipleTypes typVal

    where
      parseOneType :: JSON.Value -> JSON.Parser ValidationType
      parseOneType = fmap OneType . JSON.parseJSON

      parseMultipleTypes :: JSON.Value -> JSON.Parser ValidationType
      parseMultipleTypes = fmap MultipleTypes . JSON.parseJSON

-- {
--   "$id": "https://example.com/address.schema.json",
--   "$schema": "http://json-schema.org/draft-07/schema#",
--   "description": "An address similar to http://microformats.org/wiki/h-card",
--   "type": "object",
--   "properties": {
--     "post-office-box": {
--       "type": "string"
--     },
--     "extended-address": {
--       "type": "string"
--     },
--     "street-address": {
--       "type": "string"
--     },
--     "locality": {
--       "type": "string"
--     },
--     "region": {
--       "type": "string"
--     },
--     "postal-code": {
--       "type": "string"
--     },
--     "country-name": {
--       "type": "string"
--     }
--   },
--   "required": [ "locality", "region", "country-name" ],
--   "dependencies": {
--     "post-office-box": [ "street-address" ],
--     "extended-address": [ "street-address" ]
--   }
-- }
