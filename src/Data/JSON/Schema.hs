{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}

module Data.JSON.Schema where

import           Control.Applicative
import           Control.Monad         (when, (>=>))
import           Data.Aeson            ((.!=), (.:), (.:!), (.:?))
import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Types      as JSON
import qualified Data.Char             as Chr
import qualified Data.Foldable         as F
import           Data.Hashable
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
import qualified Data.Map.Strict       as OrdMap
import qualified Data.Maybe            as Mb
import           Data.Scientific
import           Data.Sequence         (ViewL ((:<), EmptyL), (<|))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text)
import qualified Data.Text             as Tx
import           Data.Text.Encoding    (encodeUtf8)
import qualified Data.Vector           as V
import           GHC.Generics
import qualified Network.URI           as U
import qualified Text.Regex.PCRE.Heavy as RE

data JSONSchema
  = BoolSchema Bool
  | RefSchema RefJSONSchema
  | ObjectSchema ObjectJSONSchema
  deriving (Eq, Show)

-- TODO validate the schema against its metaschema when deserializing ?
instance JSON.FromJSON JSONSchema where
  parseJSON raw
    = JSON.withBool "boolean JSONSchema" parseBool raw
    <|> JSON.withObject "object ref JSONSchema" parseRef raw
    <|> JSON.withObject "object JSONSchema" parseObject raw

    where
      parseBool b = pure $ BoolSchema b
      parseRef _o = RefSchema <$> JSON.parseJSON raw
      parseObject _o = ObjectSchema <$> JSON.parseJSON raw


data RefJSONSchema = RefJSONSchema
  { rjsURI :: URI
  , rjsId :: Maybe SchemaId
  , rjsDefinitions :: Definitions
  , rjsRawObject :: JSON.Object
  }
  deriving (Eq, Show)

instance JSON.FromJSON RefJSONSchema where
  parseJSON = JSON.withObject "RefJSONSchema" $ \o -> do
    rjsURI <- o .: "$ref"
    rjsId <- o .:? "$id"
    rjsDefinitions <- o .:? "definitions" .!= mempty
    let rjsRawObject = o
    pure RefJSONSchema{..}

data ObjectJSONSchema = ObjectJSONSchema
  { ojsDescription :: Maybe Text
  , ojsTitle :: Maybe Text
  , ojsId :: Maybe SchemaId
  , ojsVersion :: Maybe SchemaVersion
  , ojsValidators :: V.Vector Validator
  , ojsDefinitions :: Definitions
  , ojsRawObject :: JSON.Object
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ObjectJSONSchema where
  parseJSON = JSON.withObject "ObjectJSONSchema" $ \o -> do
    ojsDescription <- o .:? "description"
    ojsTitle <- o .:? "title"
    ojsId <- o .:? "$id"
    ojsVersion <- o .:? "$schema"
    ojsValidators <- parseAllValidators o
    ojsDefinitions <- o .:? "definitions" .!= mempty
    let ojsRawObject = o
    pure ObjectJSONSchema{..}


deriving instance Hashable U.URI
deriving instance Generic U.URIAuth
deriving instance Hashable U.URIAuth

newtype URI = URI { getURI :: U.URI }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Hashable, Ord)


instance JSON.FromJSON URI where
  parseJSON = JSON.withText "URI" $ \rawTxt -> do
    let rawStr = Tx.unpack rawTxt
    case U.parseURIReference rawStr of
      Nothing -> fail $ "Cannot parse URI from: " <> rawStr
      Just uri -> pure $ URI uri

instance JSON.FromJSONKey URI where
  fromJSONKey = JSON.FromJSONKeyValue JSON.parseJSON

relativeTo :: URI -> URI -> URI
relativeTo (URI u1) (URI u2) = URI (u1 `U.relativeTo` u2)

newtype SchemaId = SchemaId { getSchemaId :: URI }
  deriving stock (Eq, Show)
  deriving newtype (JSON.FromJSON)

newtype Definitions = Definitions { getDefinitions :: Map.HashMap Text JSONSchema }
  deriving stock (Eq, Show)
  deriving newtype (JSON.FromJSON, Semigroup, Monoid)

data SchemaVersion = Draft07
  deriving (Eq, Show)

instance JSON.FromJSON SchemaVersion where
  parseJSON = JSON.withText "$schema" $ \t ->
    if t == "http://json-schema.org/draft-07/schema#"
      then pure Draft07
      else fail $ "Only draft 7 is supported, got $schema = " <> Tx.unpack t

-- | For quick access to schemas when references are involved
aggregateReferences :: JSONSchema -> OrdMap.Map URI JSONSchema
aggregateReferences schema = case schema of
  BoolSchema _ -> mempty
  RefSchema _ -> mempty
  ObjectSchema o ->
    let Just emptyURI = U.parseURIReference ""
        baseURI = maybe (URI emptyURI) getSchemaId (ojsId o)
     in aggregateReferences' (Seq.singleton (baseURI, schema)) mempty


aggregateReferences'
  :: Seq.Seq (URI, JSONSchema)
  -> OrdMap.Map URI JSONSchema
  -> OrdMap.Map URI JSONSchema

aggregateReferences' seq seen = case Seq.viewl seq of
  EmptyL -> seen
  (baseURI, schema) :< rest -> if baseURI `OrdMap.member` seen
    then aggregateReferences' rest seen
    else case schema of
      BoolSchema _ -> aggregateReferences' rest seen
      RefSchema _ -> aggregateReferences' rest seen
      ObjectSchema o ->
        let baseURI' = maybe baseURI ((`relativeTo` baseURI) . getSchemaId) (ojsId o)
            seen' = OrdMap.insert baseURI schema seen
            defs = getDefinitions (ojsDefinitions o)
            f (queue, acc) k schema' =
              let uri' = addFragment ("definitions/" <> k) baseURI
                  queue' = (uri', schema') <| queue
                  acc' = case getSchemaId <$> schemaId schema' of
                    Nothing -> acc
                    Just newRoot ->
                       aggregateReferences' (Seq.singleton (newRoot `relativeTo` baseURI', schema')) acc
               in (queue', acc')
            (seq', seen2) = Map.foldlWithKey' f (rest, mempty) defs
         in aggregateReferences' seq' (OrdMap.union seen' seen2)

addFragment :: Text -> URI -> URI
addFragment frag (URI u) =
  let existingFrag = U.uriFragment u
      f = if null existingFrag
            then "#"
            else existingFrag
  in URI $ u {U.uriFragment = f <> "/" <> Tx.unpack frag}


objectSchema :: JSONSchema -> Maybe ObjectJSONSchema
objectSchema = \case
  BoolSchema _ -> Nothing
  RefSchema _ -> Nothing
  ObjectSchema o -> Just o


schemaId :: JSONSchema -> Maybe SchemaId
schemaId = \case
  BoolSchema _ -> Nothing
  RefSchema r -> rjsId r
  ObjectSchema o -> ojsId o

validators :: JSONSchema -> Maybe (V.Vector Validator)
validators = \case
  ObjectSchema o -> Just $ ojsValidators o
  _ -> Nothing

definitions :: JSONSchema -> Maybe Definitions
definitions = \case
  BoolSchema _ -> Nothing
  RefSchema s -> Just $ rjsDefinitions s
  ObjectSchema s -> Just $ ojsDefinitions s

rawObject :: JSONSchema -> Maybe JSON.Object
rawObject = \case
  BoolSchema _ -> Nothing
  RefSchema s -> Just $ rjsRawObject s
  ObjectSchema s -> Just $ ojsRawObject s

data Validator
  = ValAny AnyValidator
  | ValObject ObjectValidator
  | ValArray ArrayValidator
  | ValNumeric NumericValidator
  | ValString StringValidator
  | ValLogic LogicValidator
  | ValConditional ConditionalValidator
  | ValDependencies DependenciesValidator
  deriving (Eq, Show)

-- to avoid importing some heavy prisms
objectValidator :: Validator -> Maybe ObjectValidator
objectValidator = \case
  ValObject vs -> Just vs
  _ -> Nothing

arrayValidator :: Validator -> Maybe ArrayValidator
arrayValidator = \case
  ValArray a -> Just a
  _ -> Nothing

parseAllValidators :: JSON.Object -> JSON.Parser (V.Vector Validator)
parseAllValidators o = do
  vals <- sequence
    [ fmap ValAny <$> parseAnyValidator o
    , fmap ValObject <$> parseObjectValidator o
    , fmap ValArray <$> parseArrayValidator o
    , fmap ValNumeric <$> parseNumericValidator o
    , fmap ValString <$> parseStringValidator o
    , fmap ValLogic <$> parseLogicValidator o
    , fmap ValConditional <$> parseConditionalValidator o
    , fmap ValDependencies <$> parseDependenciesValidator o
    ]
  pure $ V.fromList $ Mb.catMaybes vals

data AnyValidator = AnyValidator
  { anyType  :: Maybe TypeValidator
  , anyEnum  :: V.Vector JSON.Value
  , anyConst :: Maybe JSON.Value
  }
  deriving (Eq, Show)

data TypeValidator
  = OneType PrimitiveType
  | MultipleTypes (V.Vector PrimitiveType)
  deriving (Eq, Show)

instance JSON.FromJSON TypeValidator where
  parseJSON raw = (OneType <$> JSON.parseJSON raw) <|> (MultipleTypes <$> JSON.parseJSON raw)

data PrimitiveType
  = PTNull
  | PTBoolean
  | PTObject
  | PTArray
  | PTNumber
  | PTInteger
  | PTString
  deriving (Eq, Show, Generic)

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

parseAnyValidator :: JSON.Object -> JSON.Parser (Maybe AnyValidator)
parseAnyValidator o = do
  anyType <- o .:? "type"
  anyEnum <- o .:? "enum" .!= mempty
  anyConst <- o .:! "const"
  if Mb.isNothing anyType && Mb.isNothing anyConst && V.null anyEnum
    then pure Nothing
    else pure $ Just $ AnyValidator{..}

data ObjectValidator = ObjectValidator
  { ovProperties      :: Map.HashMap Text JSONSchema
  , ovAdditionalProps :: AdditionalProperties
  , ovPatternProps    :: OrdMap.Map RE.Regex JSONSchema
  , ovRequired        :: RequiredProperties
  , ovMinProps        :: Maybe (Positive Int)
  , ovMaxProps        :: Maybe (Positive Int)
  , ovPropertyNames   :: Maybe JSONSchema
  }
  deriving (Eq, Show)

newtype Positive a = Positive { getPositive :: a }
  deriving stock (Eq, Show)

instance (Ord a, Num a, JSON.FromJSON a) => JSON.FromJSON (Positive a) where
  parseJSON v = do
    n <- JSON.parseJSON v
    when (n < 0) $ fail "Positive requires a positive numeric"
    pure $ Positive n

data AdditionalProperties
  = NoAdditionalProperties
  | SomeAdditionalProperties JSONSchema
  | AllAdditionalProperties
  deriving (Eq, Show)

newtype RequiredProperties = RequiredProperties
  { getRequiredProperties :: V.Vector Text }
  deriving (Eq, Show)

instance JSON.FromJSON RequiredProperties where
  parseJSON val = do
    elems <- JSON.parseJSON val
    when (length elems /= length (Set.fromList $ V.toList elems)) $
      fail "requiredProperties must all be unique"
    pure $ RequiredProperties elems

parseObjectValidator :: JSON.Object -> JSON.Parser (Maybe ObjectValidator)
parseObjectValidator o = do
  ovProperties <- o .:? "properties" .!= mempty
  ovAdditionalProps <- parseAdditionalProperties o
  ovPatternProps <- parsePatternProperties o
  ovRequired <- o .:? "required" .!= RequiredProperties mempty
  ovMinProps <- o .:? "minProperties"
  ovMaxProps <- o .:? "maxProperties"
  ovPropertyNames <- o .:? "propertyNames"

  if ovAdditionalProps == AllAdditionalProperties
       && Map.null ovProperties
       && OrdMap.null ovPatternProps
       && V.null (getRequiredProperties ovRequired)
       && Mb.isNothing ovMinProps
       && Mb.isNothing ovMaxProps
       && Mb.isNothing ovPropertyNames
    then pure Nothing
    else pure $ Just $ ObjectValidator{..}

parseAdditionalProperties :: JSON.Object -> JSON.Parser AdditionalProperties
parseAdditionalProperties o = o .:? "additionalProperties" >>= \case
  Nothing -> pure AllAdditionalProperties
  Just (JSON.Bool True) -> pure AllAdditionalProperties
  Just (JSON.Bool False) -> pure NoAdditionalProperties
  Just x -> SomeAdditionalProperties <$> JSON.parseJSON x

parsePatternProperties :: JSON.Object -> JSON.Parser (OrdMap.Map RE.Regex JSONSchema)
parsePatternProperties o = o .:? "patternProperties" >>= \case
  Nothing -> pure mempty
  Just x -> flip (JSON.withObject "patternProperties object") x $ \o' ->
    case traverse mkTuples (Map.toList o') of
      Left err -> fail err
      Right keyVals -> pure $ OrdMap.fromList keyVals

  where
    pcreOptions = []
    mkTuples :: (Text, JSON.Value) -> Either String (RE.Regex, JSONSchema)
    mkTuples (k, v) = do
      r <- RE.compileM (encodeUtf8 k) pcreOptions
      s <- JSON.parseEither JSON.parseJSON v
      pure (r, s)


data ArrayValidator = ArrayValidator
  { avMinItems        :: Maybe Int
  , avMaxItems        :: Maybe Int
  , avItems           :: ItemsValidator
  , avAdditionalItems :: AdditionalItemsValidator
  , avUniqueItems     :: UniqueItems
  , avContainsItem    :: Maybe JSONSchema
  }
  deriving (Eq, Show)

data ItemsValidator
  = SingleSchema JSONSchema
  | MultipleSchemas (V.Vector JSONSchema)
  | NoItemsValidator
  deriving (Eq, Show)

instance JSON.FromJSON ItemsValidator where
  parseJSON raw
    = (SingleSchema <$> JSON.parseJSON raw)
    <|> (MultipleSchemas <$> JSON.parseJSON raw)

data AdditionalItemsValidator
  = AdditionalSingleSchema JSONSchema
  | AdditionalMultipleSchemas (V.Vector JSONSchema)
  | AdditionalAllAllowed
  | AdditionalAllForbidden
  deriving (Eq, Show)

instance JSON.FromJSON AdditionalItemsValidator where
  parseJSON raw
    = (AdditionalSingleSchema <$> JSON.parseJSON raw)
    <|> (AdditionalMultipleSchemas <$> JSON.parseJSON raw)
    <|> JSON.withBool "additionalItems"
      (\b -> pure $ if b then AdditionalAllAllowed else AdditionalAllForbidden)
      raw

data UniqueItems
  = ItemsCanBeDuplicated
  | ItemsMustBeUnique
  deriving (Eq, Show)

instance JSON.FromJSON UniqueItems where
  parseJSON = JSON.withBool "unique items" $ \b ->
    pure $ if b
      then ItemsMustBeUnique
      else ItemsCanBeDuplicated

parseArrayValidator :: JSON.Object -> JSON.Parser (Maybe ArrayValidator)
parseArrayValidator o = do
  avMinItems <- o .:? "minItems"
  avMaxItems <- o .:? "maxItems"
  avItems <- o .:? "items" .!= NoItemsValidator
  avAdditionalItems <- o .:? "additionalItems" .!= AdditionalAllAllowed
  avUniqueItems <- o .:? "uniqueItems" .!= ItemsCanBeDuplicated
  avContainsItem <- o .:? "contains"

  case (avMinItems, avMaxItems, avItems, avAdditionalItems, avUniqueItems, avContainsItem) of
    (Nothing, Nothing, NoItemsValidator, AdditionalAllAllowed, ItemsCanBeDuplicated, Nothing)
      -> pure Nothing
    _ -> pure $ Just $ ArrayValidator{..}

data NumericValidator = NumericValidator
  { nvMultipleOf       :: Maybe Scientific
  , nvMinimum          :: Maybe Scientific
  , nvMaximum          :: Maybe Scientific
  , nvExclusiveMinimum :: Maybe Scientific
  , nvExclusiveMaximum :: Maybe Scientific
  }
  deriving (Eq, Show)

parseNumericValidator :: JSON.Object -> JSON.Parser (Maybe NumericValidator)
parseNumericValidator o = do
  nvMultipleOf       <- o .:? "multipleOf"
  nvMinimum          <- o .:? "minimum"
  nvMaximum          <- o .:? "maximum"
  nvExclusiveMinimum <- o .:? "exclusiveMinimum"
  nvExclusiveMaximum <- o .:? "exclusiveMaximum"
  case nvMultipleOf <|> nvMinimum <|> nvMaximum <|> nvExclusiveMinimum <|> nvExclusiveMaximum of
    Nothing -> pure Nothing
    Just _ -> pure $ Just NumericValidator{..}

data StringValidator = StringValidator
  { svMinLength :: Maybe (Positive Int)
  , svMaxLength :: Maybe (Positive Int)
  , svPattern   :: Maybe Pattern
  }
  deriving (Eq, Show)

data Pattern = Pattern
  { patRe :: RE.Regex
  , patRaw :: Text
  }
  deriving (Eq, Show)

instance JSON.FromJSON Pattern where
  parseJSON = JSON.withText "pattern" $ \str ->
    case RE.compileM (encodeUtf8 str) [] of
      Left err -> fail err
      Right r -> pure (Pattern r str)

parseStringValidator :: JSON.Object -> JSON.Parser (Maybe StringValidator)
parseStringValidator o = do
  svMinLength <- o .:? "minLength"
  svMaxLength <- o .:? "maxLength"
  svPattern <- o .:? "pattern"
  if Mb.isNothing svMinLength && Mb.isNothing svMaxLength && Mb.isNothing svPattern
    then pure Nothing
    else pure $ Just StringValidator{..}

data LogicValidator = LogicValidator
  { lvNot :: Maybe JSONSchema
  , lvAllOf :: V.Vector JSONSchema
  , lvAnyOf :: V.Vector JSONSchema
  , lvOneOf :: V.Vector JSONSchema
  }
  deriving (Eq, Show)

parseLogicValidator :: JSON.Object -> JSON.Parser (Maybe LogicValidator)
parseLogicValidator o = do
  lvNot <- o .:? "not"
  lvAllOf <- nonEmpty "allOf" o
  lvAnyOf <- nonEmpty "anyOf" o
  lvOneOf <- nonEmpty "oneOf" o
  if Mb.isNothing lvNot && V.null lvAllOf && V.null lvAnyOf && V.null lvOneOf
    then pure Nothing
    else pure $ Just LogicValidator{..}

  where
    nonEmpty key o = do
      mbVec <- o .:? key
      case mbVec of
        Nothing -> pure mempty
        Just vec ->
          if V.null vec
            then fail $ Tx.unpack key <> " is empty"
            else pure vec

data ConditionalValidator = ConditionalValidator
  { cvIf :: JSONSchema
  , cvThen :: Maybe JSONSchema
  , cvElse :: Maybe JSONSchema
  }
  deriving (Eq, Show)

parseConditionalValidator :: JSON.Object -> JSON.Parser (Maybe ConditionalValidator)
parseConditionalValidator o = do
  mbCvIf <- o .:? "if"
  cvThen <- o .:? "then"
  cvElse <- o .:? "else"

  case mbCvIf of
    Nothing -> pure Nothing
    -- an `if` alone has no impact on the overall validation
    Just cvIf -> if Mb.isNothing cvThen && Mb.isNothing cvElse
      then pure Nothing
      else pure $ Just ConditionalValidator{..}


newtype DependenciesValidator = DependenciesValidator (Map.HashMap Text DependencyValidator)
  deriving (Eq, Show)

data DependencyValidator
  = RequiredDependencies (V.Vector Text)
  | SchemaDependency JSONSchema
  deriving (Eq, Show)

parseDependenciesValidator :: JSON.Object -> JSON.Parser (Maybe DependenciesValidator)
parseDependenciesValidator o = do
  deps <- o .:? "dependencies" .!= mempty
  if null deps
    then pure Nothing
    else pure $ Just $ DependenciesValidator deps

instance JSON.FromJSON DependencyValidator where
  parseJSON raw
    = (RequiredDependencies <$> JSON.parseJSON raw)
    <|> (SchemaDependency <$> JSON.parseJSON raw)
