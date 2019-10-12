{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}

module Data.JSON.Validation where

import           Debug.Trace

import qualified Data.List.Split as Split
import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Reader  as Rdr
import qualified Control.Monad.State   as St
import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Types      as JSON
import qualified Data.Aeson.Text       as JSON.Tx
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Foldable         as F
import           Data.Functor          (void, ($>))
import           Data.Functor.Alt
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
import qualified Data.Map.Strict       as OrdMap
import qualified Data.Maybe            as Mb
import qualified Data.Scientific       as Scientific
import qualified Data.Set              as OrdSet
import           Data.Text             (Text)
import qualified Data.Text             as Tx
import qualified Data.Text.Lazy        as LTx
import qualified Data.Traversable      as T
import qualified Data.Typeable         as Typeable
import qualified Data.Vector           as V
import           GHC.Generics
import           Text.Regex.PCRE.Heavy ((=~))
import qualified Network.URI as U
import qualified Control.Monad.Except  as Exc
import qualified Safe

import qualified Data.JSON.Schema      as Sc

-- Data.Validation on hackage requires lens oÔ so let's roll our own simple version
data ValidationOutcome a
  = Ok a
  | Error [ValidationError]
  deriving (Eq, Show, Functor)

instance Applicative ValidationOutcome where
  pure = Ok
  f <*> x = case (f, x) of
    (Error err, _) -> Error err
    (_, Error err) -> Error err
    (Ok fun, Ok val) -> Ok (fun val)

instance Alt ValidationOutcome where
  a <!> b = case (a, b) of
    (Ok x, _) -> Ok x
    (_, Ok y) -> Ok y
    (Error a, _) -> Error a
  {-# INLINE (<!>) #-}

instance Alternative ValidationOutcome where
  a <|> b = a <!> b
  empty = Error mempty

instance F.Foldable ValidationOutcome where
  foldr f x (Ok a) = f a x
  foldr _ x (Error _) = x
  {-# INLINE foldr #-}

instance T.Traversable ValidationOutcome where
  traverse f x = case x of
    (Ok a) -> Ok <$> f a
    (Error err) -> pure (Error err)
  {-# INLINE traverse #-}

prettyValidationOutcome :: Show a => ValidationOutcome a -> Text
prettyValidationOutcome = \case
  Ok a -> "Ok " <> Tx.pack (show a)
  Error errors -> "Errors: [" <> Tx.intercalate " - " (fmap prettyError errors) <> "]"

  where
    prettyError :: ValidationError -> Text
    prettyError err = Tx.intercalate ", "
      [ "keyword: \"" <> verrKeyword err <> "\""
      , "message: \"" <> verrMessage err <> "\""
      , "dataPath: \"." <> Tx.intercalate "." (reverse $ verrDataPath err) <> "\""
      , "schemaPath: \"#/" <> Tx.intercalate "/" (reverse $ verrSchemaPath err) <> "\""
      , "params: " <> LTx.toStrict (JSON.Tx.encodeToLazyText $ verrParams err)
      ]

data ValidationError = ValidationError
  { verrKeyword :: Text
  , verrMessage :: Text
  , verrParams  :: JSON.Value
  , verrDataPath :: [Text]
  , verrSchemaPath :: [Text]
  }
  deriving (Eq, Show, Generic, JSON.ToJSON)

newtype ValidationM a = ValidationM
  { runValidationM :: Rdr.ReaderT ValidationEnv (St.State ValidationState) a
  }
  deriving newtype
    ( Functor, Applicative, Monad
    , Rdr.MonadReader ValidationEnv
    , St.MonadState ValidationState
    )

type ValM a = ValidationM (ValidationOutcome a)

mkValidationError :: Text -> Text -> JSON.Value -> ValM a
mkValidationError keyword msg params = (\e -> Error [e]) <$> mkValidationError' keyword msg params

mkValidationError' keyword msg params = Rdr.ask >>= \env -> pure
  ValidationError
    { verrKeyword    = keyword
    , verrMessage    = msg
    , verrParams     = params
    , verrDataPath   = veDataPath env
    , verrSchemaPath = veSchemaPath env
    }

valMFromEither :: Either [ValidationError] a -> ValM a
valMFromEither = \case
  Left e -> pure $ Error e
  Right a -> pure $ Ok a

data ValidationEnv = ValidationEnv
  { veDataPath :: [Text]
  , veSchemaPath :: [Text]
  , veReferences :: OrdMap.Map Sc.URI Sc.JSONSchema
  , veRootSchema :: Sc.JSONSchema
  }
  deriving (Eq, Show)

type ValidationState = Map.HashMap (Sc.URI, [Text]) (Set.HashSet JSON.Value)

addSchemaPath, addDataPath :: Text -> ValidationEnv -> ValidationEnv
addSchemaPath path env = env {veSchemaPath = path : veSchemaPath env}
addDataPath path env = env {veDataPath = path : veDataPath env}

runValidation :: ValidationM a -> ValidationEnv -> a
runValidation m env = St.evalState (Rdr.runReaderT (runValidationM m) env) mempty

validateSchema :: Sc.JSONSchema -> JSON.Value -> ValidationOutcome JSON.Value
validateSchema schema val =
  let env = ValidationEnv
        { veDataPath = []
        , veSchemaPath = []
        -- only collect references from the root schema
        , veReferences = Sc.aggregateReferences schema
        , veRootSchema = schema
        }
   in runValidation (validate schema val) env

-- TODO take care of infinite loop
validate :: Sc.JSONSchema -> JSON.Value -> ValM JSON.Value
validate schema val = case schema of
  Sc.BoolSchema b -> if b
    then pure (Ok val)
    else mkValidationError "false schema" "boolean schema is false" (JSON.Object mempty)
  Sc.RefSchema r -> validateRefSchema r val
  Sc.ObjectSchema schema -> do
    valResults <- traverse (validateObjectSchema val) (Sc.ojsValidators schema)
    pure $ T.sequenceA valResults $> val

validateRefSchema :: Sc.RefJSONSchema -> JSON.Value -> ValM JSON.Value
validateRefSchema rs val = do
  rootSchema <- Rdr.asks veRootSchema
  let uri = Sc.rjsURI rs
  let mbRootUri = Sc.schemaId rootSchema
  let refUri = maybe uri ((uri `Sc.relativeTo`) . Sc.getSchemaId) (Sc.schemaId rootSchema)

  -- TODO switch schema if base+path isn't the same as root schema

  traceM $ "refUri: " <> show refUri
  traceM $ "own uri: " <> show uri
  traceM $ "root uri: " <> show mbRootUri

  seen <- St.get
  dataPath <- Rdr.asks veDataPath
  let hasLoop = maybe False (Set.member val) (Map.lookup (refUri, dataPath) seen)

  if hasLoop || isRemoteSchema mbRootUri refUri
    then
      if hasLoop
        then mkValidationError "$ref" "Infinite loop detected" val
        else error "remote schema"
    else do
      let seen' = Map.insertWith Set.union (refUri, dataPath) (Set.singleton val) seen
      St.put seen'
      allReferences <- Rdr.asks veReferences

      let fragment = U.uriFragment $ Sc.getURI refUri
      case OrdMap.lookup refUri allReferences of
        Just absoluteSchema -> validate absoluteSchema val
        Nothing -> do -- relative reference or json path
      -- if not (null $ U.uriScheme $ Sc.getURI refUri)
      --   then error $ "root schema id: " <> show mbRootUri <> " ref schema uri: " <> show refUri <> " and all refs: " <> show (OrdMap.keys allReferences) -- error "doesn't support absolute schema ref yet"
      --   else do
          let byId = note
                ("No schema found for absolute reference " <> showT refUri)
                (OrdMap.lookup refUri allReferences)
          let byFragment = resolveFragment rootSchema fragment

          -- Annoyingly, `Either Text` doesn't have an alternative instance
          let eitherSchema = either (const byFragment) Right byId
          case eitherSchema of
            Left err -> mkValidationError
              "$ref" ("No schema found for fragment " <> Tx.pack fragment <> " because " <> err) val
            Right s -> validate s val

  where
    withoutFragment (Sc.URI u) = Sc.URI $ u {U.uriFragment = ""}
    isRemoteSchema mbRootUri (Sc.URI refUri) = case mbRootUri of
      Nothing -> not $ null (U.uriScheme refUri) && null (maybe "" U.uriRegName $ U.uriAuthority refUri)
      Just (Sc.SchemaId (Sc.URI rootUri)) -> not $
        U.uriScheme rootUri == U.uriScheme refUri
        && U.uriAuthority rootUri == U.uriAuthority refUri

resolveFragment :: Sc.JSONSchema -> String -> Either Text Sc.JSONSchema
resolveFragment schema fragment =
  case chunkFrag fragment of
    ("", "") -> Right schema
    ("properties", rest) -> do
      let (prop, rest') = chunkFrag rest
      vs <- note "No validators" $ Sc.validators schema
      ovs <- note "No item validator found" $ F.asum (fmap Sc.objectValidator vs)
      nextSchema <- note ("No schema found under key " <> showT prop) $ Map.lookup prop (Sc.ovProperties ovs)
      resolveFragment nextSchema rest'

    ("items", rest) -> do
      let (strIdx, rest') = chunkFrag rest
      vs <- note "No validator found" $ Sc.validators schema
      avs <- note "No array validator found" $ F.asum (fmap Sc.arrayValidator vs)
      idx <- note ("Invalid index: " <> strIdx) $ Safe.readMay $ Tx.unpack strIdx
      case Sc.avItems avs of
        Sc.SingleSchema s -> if idx == 0
          then resolveFragment s rest'
          else Left ("No item schema found for index " <> strIdx)
        Sc.MultipleSchemas ss -> do
          nextSchema <- note ("No item schema found for index " <> strIdx) $ ss V.!? idx
          resolveFragment nextSchema rest'
        Sc.NoItemsValidator -> Left ("no items validator found for " <> Tx.pack fragment)

    (kw, rest) -> if kw == "definitions"
      then do
        definitions <- note "No definitions found" $ Sc.getDefinitions <$> Sc.definitions schema
        let (def, remainingFrag) = chunkFrag rest
        let properDef = unEscape def
        case Map.lookup properDef definitions of
          Just nextSchema -> resolveFragment nextSchema remainingFrag
          Nothing -> Left $ "No definition found for: " <> def <> " (real ref searched: " <> properDef <> ")"
      else do
        os <- note "No json object found" $ Sc.rawObject schema
        note
          ("No schema found for json pointer" <> Tx.pack fragment)
          (findSchema (JSON.Object os) (Tx.pack fragment))

  where
    sanitize = \case
      [] -> []
      ('/' : rest) -> rest
      ('#' : rest) -> sanitize rest
      frag -> frag

    chunkFrag str =
      let (h, t) = span (/= '/') (sanitize str)
       in (unEscape $ Tx.pack h, t)


findSchemaByAbsoluteRef
  :: OrdMap.Map Sc.URI Sc.JSONSchema
  -> Sc.URI
  -> Maybe Sc.JSONSchema

findSchemaByAbsoluteRef =
  let
  in error "foo"


unEscape :: Text -> Text
unEscape str = if Tx.null str
  then str
  else
    Tx.replace "~0" "~"
    $ Tx.replace "~1" "/"
    $ Tx.replace "%22" "\""
    $ Tx.replace "%25" "%" str


-- given a json pointer, try to drill down and get a schema at the location
findSchema :: JSON.Value -> Text -> Maybe Sc.JSONSchema
findSchema val pointer =
  let (firstChar, rest) = Tx.splitAt 2 pointer
   in if firstChar /= "#/"
        then Nothing
        else go val (Tx.split (== '/') rest)

  where
    go val = \case
      [] -> JSON.parseMaybe JSON.parseJSON val
      (pointer : rest) -> case val of
        JSON.Object obj -> do
          val' <- Map.lookup (unEscape pointer) obj
          go val' rest
        JSON.Array arr -> do
          idx <- Safe.readMay (Tx.unpack pointer)
          val' <- arr V.!? idx
          go val' rest
        _ -> Nothing

validateObjectSchema :: JSON.Value -> Sc.Validator -> ValM ()
validateObjectSchema value = \case
  Sc.ValAny valAny -> validateAny value valAny
  Sc.ValObject valObj -> validateObject value valObj
  Sc.ValArray valA -> validateArray value valA
  Sc.ValNumeric valN -> validateNumeric value valN
  Sc.ValString valStr -> validateString value valStr
  Sc.ValLogic valLogic -> validateLogic value valLogic

runMbValidator :: (a -> ValM ()) -> Maybe a -> ValM ()
runMbValidator = maybe (pure $ pure ())

validateAny :: JSON.Value -> Sc.AnyValidator -> ValM ()
validateAny value valAny = F.sequenceA_ <$> sequence
  [ runMbValidator (validateType value) (Sc.anyType valAny)
  , runMbValidator (validateConst value) (Sc.anyConst valAny)
  , validateEnum value (Sc.anyEnum valAny)
  ]

  where
    validateConst val constVal = Rdr.local (addSchemaPath "const") $
      if val == constVal
        then pure $ Ok ()
        else mkValidationError
          "const"
          "should be equal to constant"
          (JSON.Object $ Map.singleton "allowedValue" constVal)

    validateEnum val possibleVals = Rdr.local (addSchemaPath "enum") $
      if V.null possibleVals || F.any (== val) possibleVals
        then pure $ Ok ()
        else mkValidationError
          "enum"
          "should be equal to one of the allowed value"
          val

validateType :: JSON.Value -> Sc.TypeValidator -> ValM ()
validateType value valType = Rdr.local (addSchemaPath "type") $
  let acceptableTypes = case valType of
        Sc.OneType t -> V.singleton t
        Sc.MultipleTypes typs -> typs
      want t = if t `V.elem` acceptableTypes
        then pure $ Ok ()
        else mkError acceptableTypes t
  in case value of
      JSON.Object _ -> want Sc.PTObject
      JSON.Array _  -> want Sc.PTArray
      JSON.String _ -> want Sc.PTString
      JSON.Bool _   -> want Sc.PTBoolean
      JSON.Null     -> want Sc.PTNull
      JSON.Number n -> if Scientific.isInteger n
        then do
          a <- want Sc.PTInteger
          b <- want Sc.PTNumber
          pure $ a <!> b
        else want Sc.PTNumber

  where
    mkError :: V.Vector Sc.PrimitiveType -> Sc.PrimitiveType -> ValM ()
    mkError typs t = mkValidationError
      "type"
      ("should be: " <> Tx.intercalate ", " (V.toList $ fmap Sc.prettyPrimitiveType typs))
      (JSON.Object $ Map.singleton
        "allowedTypes"
        (JSON.Array $ fmap (JSON.String . Sc.prettyPrimitiveType) typs))


validateObject :: JSON.Value -> Sc.ObjectValidator -> ValM ()
validateObject value valObj = case value of
  JSON.Object o -> F.sequenceA_ <$> sequence
    [ F.sequenceA_ <$> Map.traverseWithKey (validateProps (Sc.ovProperties valObj)) o
    , validateAdditionalProps o valObj
    , validatePatternProps o (Sc.ovPatternProps valObj)
    , validateRequired o (Sc.ovRequired valObj)
    , runMbValidator (validateMinProps o) (Sc.ovMinProps valObj)
    , runMbValidator (validateMaxProps o) (Sc.ovMaxProps valObj)
    , runMbValidator (validatePropertyNames o) (Sc.ovPropertyNames valObj)
    ]

  _ -> pure $ pure ()

  where
    validateProps :: Map.HashMap Text Sc.JSONSchema -> Text -> JSON.Value -> ValM ()
    validateProps props key jsonVal = Rdr.local (addDataPath key . addSchemaPath "properties") $ case Map.lookup key props of
      Nothing -> pure $ pure ()
      Just schema -> void <$> validate schema jsonVal

    validateAdditionalProps :: JSON.Object -> Sc.ObjectValidator -> ValM ()
    validateAdditionalProps o valObj =
      let additionalKeyVals = getAdditionalKeyValues o valObj
      in case Sc.ovAdditionalProps valObj of
        Sc.AllAdditionalProperties -> pure $ Ok ()
        Sc.NoAdditionalProperties -> if null additionalKeyVals
          then pure $ Ok ()
          else mkValidationError
            "additionalProperties"
            "should NOT have additionalProperties"
            (JSON.Object $ Map.fromList additionalKeyVals)

        Sc.SomeAdditionalProperties schema -> do
          result <- traverse
            (\(k, v) -> Rdr.local (addDataPath k) $ validate schema v)
            additionalKeyVals
          pure $ F.sequenceA_ result

    validatePatternProps o patternProps = Rdr.local (addSchemaPath "patternProperties") $ do
      results <- traverse (validateOnePatternProp o) (OrdMap.toList patternProps)
      pure $ F.traverse_ (T.sequenceA . Map.elems) results

    validateOnePatternProp o (regexp, schema) = Map.traverseWithKey
      (\k v -> if k =~ regexp
                 then Rdr.local (addDataPath k) $ void <$> validate schema v
                 else pure $ Ok ()
      )
      o

    -- additionalProperties and patternProperties interaction requires some logic to get all
    -- keys covered neither by `properties` nor by `patternProperties`
    getAdditionalKeyValues :: JSON.Object -> Sc.ObjectValidator -> [(Text, JSON.Value)]
    getAdditionalKeyValues obj valObj =
      let existingKeys = OrdSet.fromList $ Map.keys obj
          allowedKeys = OrdSet.fromList (Map.keys $ Sc.ovProperties valObj)
          allAdditionalKeys = OrdSet.difference existingKeys allowedKeys
          allPatterns = OrdMap.keys (Sc.ovPatternProps valObj)
          additionalKeys = filter (\k -> not $ F.any (k =~) allPatterns) (OrdSet.toList allAdditionalKeys)
      in Mb.mapMaybe (\k -> (k,) <$> Map.lookup k obj) additionalKeys

    validateRequired o (Sc.RequiredProperties req) = Rdr.local (addSchemaPath "required") $
      if F.all (`Map.member` o) req
        then pure $ Ok ()
        else mkValidationError
          "required"
          ("should have required properties " <> Tx.intercalate ", " (V.toList req))
          (JSON.Object $ Map.singleton "requiredProperties" (JSON.Array $ fmap JSON.String req))

    validateNum pred keyword msg o (Sc.Positive limit) = Rdr.local (addSchemaPath keyword) $
      if pred (length $ Map.keys o) limit
        then pure $ Ok ()
        else mkValidationError
          keyword
          (msg <> " " <> Tx.pack (show limit) <> " properties")
          (JSON.Object $ Map.singleton "limit" (mkJSONNum limit))

    validateMinProps = validateNum (>=) "minProperties" "should NOT have fewer than"
    validateMaxProps = validateNum (<=) "maxProperties" "should NOT have less than"

    validatePropertyNames o schema = Rdr.local (addSchemaPath "propertyNames") $ do
      results <- forM (Map.keys o) $ \k -> do
        res <- validate schema (JSON.String k)
        case res of
          Ok _ -> pure $ Ok ()
          Error errs -> do
            nameError <- mkValidationError'
              "propertyNames"
              ("property name " <> k <> " is invalid")
              (JSON.Object $ Map.singleton "propertyNames" (JSON.String k))
            pure $ Error (nameError : errs)

      pure $ F.sequenceA_ results


validateArray :: JSON.Value -> Sc.ArrayValidator -> ValM ()
validateArray value valA = case value of
  JSON.Array arr -> F.sequenceA_ <$> sequence
    [ runMbValidator (validateMaxItems arr) (Sc.avMaxItems valA)
    , runMbValidator (validateMinItems arr) (Sc.avMinItems valA)
    , validateItems arr valA
    , validateUnique arr (Sc.avUniqueItems valA)
    , runMbValidator (validateContains arr) (Sc.avContainsItem valA)
    ]

  _ -> pure $ Ok ()

  where
    validateMaxItems a limit = Rdr.local (addSchemaPath "maxItems") $
      if V.length a <= limit
        then pure $ Ok ()
        else mkValidationError
          "maxItems"
          ("should NOT have more than " <> Tx.pack (show limit) <> " items")
          (JSON.Object $ Map.singleton "limit" (mkJSONNum limit))

    validateMinItems a limit = Rdr.local (addSchemaPath "minItems") $
      if V.length a >= limit
        then pure $ Ok ()
        else mkValidationError
          "minItems"
          ("should NOT have less than " <> Tx.pack (show limit) <> " items")
          (JSON.Object $ Map.singleton "limit" (mkJSONNum limit))

    iTraverse :: Applicative f => ((Int, a) -> f b) -> V.Vector a -> f (V.Vector b)
    iTraverse f vec =
      let idxs = V.iterateN (V.length vec) (+1) 0
       in traverse f (V.zip idxs vec)

    validateItems a v = Rdr.local (addSchemaPath "items") $ case Sc.avItems v of
      Sc.SingleSchema schema -> do
        results <- iTraverse
          (\(idx, val) -> Rdr.local (addDataPath $ Tx.pack (show idx)) $ validate schema val)
          a
        pure $ F.sequenceA_ results

      Sc.MultipleSchemas schemas -> Rdr.local (addSchemaPath "items") $ if V.length a <= V.length schemas
        then do
          results <- iTraverse
            (\(idx, (schema, val)) -> Rdr.local (addDataPath $ Tx.pack (show idx)) $
              validate schema val)
            (V.zip schemas a)
          pure $ F.sequenceA_ results
        else
          let remainingItems = V.drop (V.length schemas) a
              schemasL = V.length schemas
           in case Sc.avAdditionalItems v of
                Sc.AdditionalAllAllowed
                  -> pure $ Ok ()

                Sc.AdditionalAllForbidden
                  -> mkValidationError
                       "additionalItems"
                       ("should NOT have more than " <> Tx.pack (show schemasL) <> " items")
                       (JSON.Object $ Map.singleton "limit" (mkJSONNum schemasL))

                Sc.AdditionalSingleSchema schema
                  -> do
                    results <- iTraverse
                      (\(idx, val) -> Rdr.local (addDataPath $ Tx.pack (show $ idx + V.length schemas)) $
                        validate schema val)
                      remainingItems
                    pure $ F.sequenceA_ results


                Sc.AdditionalMultipleSchemas schemas'
                  -> if V.length remainingItems <= V.length schemas'
                       then do
                         results <- iTraverse
                           (\(idx, (s, val)) -> Rdr.local (addDataPath $ Tx.pack (show $ idx + schemasL)) $
                             validate s val)
                           (V.zip schemas' remainingItems)
                         pure $ F.sequenceA_ results
                       else mkValidationError
                         "additionalItems"
                         ("should NOT have more than " <> Tx.pack (show schemasL) <> " items")
                         (JSON.Object $ Map.singleton "limit" (mkJSONNum schemasL))

      Sc.NoItemsValidator -> pure $ Ok ()

    validateUnique a v = Rdr.local (addSchemaPath "uniqueItems") $ case v of
      Sc.ItemsCanBeDuplicated -> pure $ Ok ()
      Sc.ItemsMustBeUnique -> if length (Set.fromList $ V.toList a) == length a
        then pure $ Ok ()
        else mkValidationError
          "uniqueItems"
          "should NOT have duplicate items"
          (JSON.Object mempty)

    validateContains a schema = Rdr.local (addSchemaPath "contains") $ do
      results <- mapM (validate schema) a
      pure $ void $ F.asum results

validateNumeric :: JSON.Value -> Sc.NumericValidator -> ValM ()
validateNumeric value valN = case value of
  JSON.Number n -> F.sequenceA_ <$> sequence
    [ runMbValidator (validateMultipleOf n) (Sc.nvMultipleOf valN)
    , runMbValidator (validateMinimum n) (Sc.nvMinimum valN)
    , runMbValidator (validateMaximum n) (Sc.nvMaximum valN)
    , runMbValidator (validateExclusiveMinimum n) (Sc.nvExclusiveMinimum valN)
    , runMbValidator (validateExclusiveMaximum n) (Sc.nvExclusiveMaximum valN)
    ]

  _ -> pure $ Ok ()

  where
    validateMultipleOf :: Scientific.Scientific -> Scientific.Scientific -> ValM ()
    validateMultipleOf n x = Rdr.local (addSchemaPath "multipleOf") $
      let (coeffN, baseN) = (Scientific.coefficient n, Scientific.base10Exponent n)
          (coeffX, baseX) = (Scientific.coefficient x, Scientific.base10Exponent x)
          assertMult b = if b
            then pure $ Ok ()
            else mkValidationError
              "multipleOf"
              (Tx.pack (show n) <> " is not a multiple of " <> Tx.pack (show x))
              (JSON.Number n)

       in if baseN == baseX
            then assertMult (coeffN `mod` coeffX == 0)
            else
              let diff = abs (baseN - baseX)
                  (cn', cx') = if baseN > baseX
                    then (coeffN * 10 ^ diff, coeffX)
                    else (coeffN, coeffX * 10 ^ diff)
               in assertMult (cn' `mod` cx' == 0)


    assert pred keyword msg n limit = Rdr.local (addSchemaPath keyword) $
      if pred n limit
        then pure $ Ok ()
        else mkValidationError
          keyword
          (Tx.pack (show n) <> " " <> msg <> " " <> Tx.pack (show limit))
          (JSON.Number n)

    validateMinimum = assert (>=) "minimum" "must be greater than"
    validateMaximum = assert (<=) "maximum" "must be less than"
    validateExclusiveMinimum = assert (>) "exclusiveMinimum" "must be strictly greater than"
    validateExclusiveMaximum = assert (<) "exclusiveMaximum" "must be strictly less than"

validateString :: JSON.Value -> Sc.StringValidator -> ValM ()
validateString value valStr = case value of
  JSON.String str -> F.sequenceA_ <$> sequence
    [ runMbValidator (validateMinLength str) (Sc.svMinLength valStr)
    , runMbValidator (validateMaxLength str) (Sc.svMaxLength valStr)
    , runMbValidator (validatePattern str) (Sc.svPattern valStr)
    ]

  _ -> pure $ Ok ()

  where
    validateNumeric pred keyword msg s (Sc.Positive limit) = Rdr.local (addSchemaPath keyword) $
      if pred (Tx.length s) limit
        then pure $ Ok ()
        else mkValidationError
          keyword
          (msg <> " " <> Tx.pack (show limit) <> " characters")
          (JSON.Object $ Map.singleton "limit" (mkJSONNum limit))

    validateMinLength = validateNumeric (>=) "minLength" "should NOT be shorter than"
    validateMaxLength = validateNumeric (<=) "minLength" "should NOT be longer than"

    validatePattern str (Sc.Pattern pat rawPattern) = Rdr.local (addSchemaPath "pattern") $
      if str =~ pat
        then pure $ Ok ()
        else mkValidationError
          "pattern"
          ("should match pattern " <> rawPattern)
          (JSON.Object $ Map.singleton "pattern" (JSON.String rawPattern))


validateLogic :: JSON.Value -> Sc.LogicValidator -> ValM ()
validateLogic value valLogic = F.sequenceA_ <$> sequence
  [ runMbValidator (validateNot value) (Sc.lvNot valLogic)
  , runWhenNonEmpty (validateAllOf value) (Sc.lvAllOf valLogic)
  , runWhenNonEmpty (validateAnyOf value) (Sc.lvAnyOf valLogic)
  , runWhenNonEmpty (validateOneOf value) (Sc.lvOneOf valLogic)
  ]

  where
    validateNot val schema = Rdr.local (addSchemaPath "not") $
      validate schema val >>= \case
        Error _ -> pure $ Ok ()
        Ok _ -> mkValidationError
          "not"
          "should NOT be valid"
          (JSON.Object mempty)

    validateAllOf val schemas = Rdr.local (addSchemaPath "allOf") $ do
      results <- traverse (`validate` val) schemas
      pure $ F.sequenceA_ results

    validateAnyOf val schemas = Rdr.local (addSchemaPath "anyOf") $ do
      results <- mapM (`validate` val) schemas
      pure $ void $ F.asum results

    validateOneOf val schemas = Rdr.local (addSchemaPath "oneOf") $ do
      results <- mapM (`validate` val) schemas
      let withIdx = V.zip (V.iterateN (V.length results) (+1) 0) results
      let passingWithIdx = V.filter (isOk . snd) withIdx
      if length passingWithIdx == 1
        then pure $ Ok ()
        else mkValidationError
          "oneOf"
          "should match exactly one schema in oneOf"
          (JSON.Object $ Map.singleton "passingSchemas" $ JSON.Array (fmap (mkJSONNum . fst) passingWithIdx))

    runWhenNonEmpty f a = if V.null a
      then pure $ Ok ()
      else f a

    isOk = \case
      Ok{} -> True
      Error{} -> False


mkJSONNum :: Int -> JSON.Value
mkJSONNum x = JSON.Number $ Scientific.scientific (fromIntegral x) 0



hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right x) = Just x

note :: e -> Maybe a -> Either e a
note msg Nothing = Left msg
note _ (Just x) = Right x

showT :: Show a => a -> Text
showT = Tx.pack . show
