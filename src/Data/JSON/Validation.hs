{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}

module Data.JSON.Validation where

import Debug.Trace

import GHC.Generics
import qualified Data.Maybe as Mb
import           Text.Regex.PCRE.Heavy ((=~))
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Text     as JSON.Tx
import qualified Data.Foldable       as F
import           Data.Functor        (void, ($>))
import           Data.Functor.Alt
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict     as OrdMap
import qualified Data.Scientific     as Scientific
import qualified Data.Set            as OrdSet
import qualified Data.HashSet        as Set
import           Data.Text           (Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text           as Tx
import qualified Data.Text.Lazy      as LTx
import qualified Data.Traversable    as T
import qualified Data.Typeable       as Typeable
import qualified Data.Vector         as V
import Control.Applicative
import qualified Control.Monad.Reader as Rdr

import qualified Data.JSON.Schema    as Sc

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

mkValidationError :: Text -> Text -> JSON.Value -> ValM a
mkValidationError keyword msg params = Rdr.ask >>= \env -> pure $ Error [ValidationError
  { verrKeyword    = keyword
  , verrMessage    = msg
  , verrParams     = params
  , verrDataPath   = veDataPath env
  , verrSchemaPath = veSchemaPath env
  }]

data ValidationEnv = ValidationEnv
  { veDataPath :: [Text]
  , veSchemaPath :: [Text]
  }
  deriving (Eq, Show)

emptyValidationEnv :: ValidationEnv
emptyValidationEnv = ValidationEnv [] []

addSchemaPath, addDataPath :: Text -> ValidationEnv -> ValidationEnv
addSchemaPath path env = env {veSchemaPath = path : veSchemaPath env}
addDataPath path env = env {veDataPath = path : veDataPath env}

newtype ValidationM a = ValidationM { runValidationM :: Rdr.Reader ValidationEnv a }
  deriving newtype (Functor, Applicative, Monad, Rdr.MonadReader ValidationEnv)

type ValM a = ValidationM (ValidationOutcome a)

runValidation :: ValidationM a -> ValidationEnv -> a
runValidation m = Rdr.runReader (runValidationM m)

runMbValidator :: (a -> ValM ()) -> Maybe a -> ValM ()
runMbValidator = maybe (pure $ pure ())

validate :: Sc.JSONSchema -> JSON.Value -> ValM JSON.Value
validate schema val = do
  valResults <- traverse (validate' val) (Sc.sValidators $ Sc.schema schema)
  pure $ traverse id valResults $> val

validate' :: JSON.Value -> Sc.Validator -> ValM ()
validate' value = \case
  Sc.ValAny valAny -> validateAny value valAny
  Sc.ValObject valObj -> validateObject value valObj
  Sc.ValBool b -> validateBoolean b
  Sc.ValArray valA -> validateArray value valA
  Sc.ValNumeric valN -> validateNumeric value valN


validateAny :: JSON.Value -> Sc.AnyValidator -> ValM ()
validateAny value valAny = do
  resultType <- runMbValidator (validateType value) (Sc.anyType valAny)
  resultConst <- runMbValidator (validateConst value) (Sc.anyConst valAny)
  resultEnum <- validateEnum value (Sc.anyEnum valAny)

  pure $ F.traverse_ id
    [ resultType
    , resultConst
    , resultEnum
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
  JSON.Object o -> do
    resultProp <- F.traverse_ id <$> Map.traverseWithKey (validateProps (Sc.ovProperties valObj)) o
    resultAdditionalProps <- validateAdditionalProps o valObj
    resultPatternProps <- validatePatternProps o (Sc.ovPatternProps valObj)
    resultRequired <- validateRequired o (Sc.ovRequired valObj)
    resultMinProps <- runMbValidator (validateMinProps o) (Sc.ovMinProps valObj)
    resultMaxProps <- runMbValidator (validateMaxProps o) (Sc.ovMaxProps valObj)

    pure $ F.traverse_ id
      [ resultProp
      , resultAdditionalProps
      , resultPatternProps
      , resultRequired
      , resultMinProps
      , resultMaxProps
      ]
  _ -> pure $ pure ()

  where
    validateProps :: Map.HashMap Text Sc.Schema -> Text -> JSON.Value -> ValM ()
    validateProps props key jsonVal = Rdr.local (addDataPath key . addSchemaPath "properties") $ case Map.lookup key props of
      Nothing -> pure $ pure ()
      Just schema -> void <$> validate (Sc.SubSchema schema) jsonVal

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
            (\(k, v) -> Rdr.local (addDataPath k) $ validate (Sc.SubSchema schema) v)
            additionalKeyVals
          pure $ F.traverse_ id result

    validatePatternProps o patternProps = Rdr.local (addSchemaPath "patternProperties") $ do
      results <- traverse (validateOnePatternProp o) (OrdMap.toList patternProps)
      pure $ F.traverse_ (traverse id . Map.elems) results

    validateOnePatternProp o (regexp, schema) = Map.traverseWithKey
      (\k v -> if k =~ regexp
                 then Rdr.local (addDataPath k) $ void <$> validate (Sc.SubSchema schema) v
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

    validateNum pred keyword msg o limit = Rdr.local (addSchemaPath keyword) $
      if pred (length $ Map.keys o) limit
        then pure $ Ok ()
        else mkValidationError
          keyword
          (msg <> " " <> Tx.pack (show limit) <> " properties")
          (JSON.Object $ Map.singleton "limit" (mkJSONNum limit))

    validateMinProps = validateNum (>=) "minProperties" "should NOT have fewer than"
    validateMaxProps = validateNum (<=) "maxProperties" "should NOT have less than"


validateBoolean :: Bool -> ValM ()
validateBoolean b = if b
  then pure $ Ok ()
  else mkValidationError "false schema" "boolean schema is false" (JSON.Object mempty)

validateArray :: JSON.Value -> Sc.ArrayValidator -> ValM ()
validateArray value valA = case value of
  JSON.Array arr -> do
    resultMaxItems <- runMbValidator (validateMaxItems arr) (Sc.avMaxItems valA)
    resultMinItems <- runMbValidator (validateMinItems arr) (Sc.avMinItems valA)
    resultItems <- validateItems arr valA
    resultUnique <- validateUnique arr (Sc.avUniqueItems valA)
    resultContains <- runMbValidator (validateContains arr) (Sc.avContainsItem valA)

    pure $ F.traverse_ id
      [ resultMaxItems
      , resultMinItems
      , resultItems
      , resultUnique
      , resultContains
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

    -- validateItems :: JSON.Array -> Sc.ItemsValidator -> ValM ()
    validateItems a v = Rdr.local (addSchemaPath "items") $ case Sc.avItems v of
      Sc.SingleSchema schema -> do
        results <- iTraverse
          (\(idx, val) -> Rdr.local (addDataPath $ Tx.pack (show idx)) $ validate (Sc.SubSchema schema) val)
          a
        pure $ F.traverse_ id results

      Sc.MultipleSchemas schemas -> Rdr.local (addSchemaPath "items") $ if V.length a <= V.length schemas
        then do
          results <- iTraverse
            (\(idx, (schema, val)) -> Rdr.local (addDataPath $ Tx.pack (show idx)) $
              validate (Sc.SubSchema schema) val)
            (V.zip schemas a)
          pure $ F.traverse_ id results
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
                        validate (Sc.SubSchema schema) val)
                      remainingItems
                    pure $ F.traverse_ id results


                Sc.AdditionalMultipleSchemas schemas'
                  -> if V.length remainingItems <= V.length schemas'
                    then do
                      results <- iTraverse
                        (\(idx, (s, val)) -> Rdr.local (addDataPath $ Tx.pack (show $ idx + schemasL)) $
                          validate (Sc.SubSchema s) val)
                        (V.zip schemas' remainingItems)
                      pure $ F.traverse_ id results
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
      results <- mapM (validate (Sc.SubSchema schema)) a
      pure $ void $ F.asum results

validateNumeric :: JSON.Value -> Sc.NumericValidator -> ValM ()
validateNumeric value valN = case value of
  JSON.Number n -> do
    resultMultipleOf <- runMbValidator (validateMultipleOf n) (Sc.nvMultipleOf valN)
    resultMinimum <- runMbValidator (validateMinimum n) (Sc.nvMinimum valN)
    resultMaximum <- runMbValidator (validateMaximum n) (Sc.nvMaximum valN)
    resultExclusiveMinimum <- runMbValidator (validateExclusiveMinimum n) (Sc.nvExclusiveMinimum valN)
    resultExclusiveMaximum <- runMbValidator (validateExclusiveMaximum n) (Sc.nvExclusiveMaximum valN)

    pure $ F.traverse_ id
      [ resultMultipleOf
      , resultMinimum
      , resultMaximum
      , resultExclusiveMinimum
      , resultExclusiveMaximum
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


mkJSONNum :: Int -> JSON.Value
mkJSONNum x = JSON.Number $ Scientific.scientific (fromIntegral x) 0
