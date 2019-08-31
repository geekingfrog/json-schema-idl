{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSON.Validation where

import qualified Data.Maybe as Mb
import           Text.Regex.PCRE.Heavy ((=~))
import qualified Data.Aeson          as JSON
import qualified Data.Foldable       as F
import           Data.Functor        (void, ($>))
import           Data.Functor.Alt
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict     as OrdMap
import qualified Data.Scientific     as Scientific
import qualified Data.Set            as OrdSet
import qualified Data.HashSet        as Set
import           Data.Text           (Text)
import qualified Data.Text           as Tx
import qualified Data.Traversable    as T
import qualified Data.Typeable       as Typeable
import qualified Data.Vector         as V

import qualified Data.JSON.Schema    as Sc

-- Data.Validation on hackage requires lens oÔ so let's roll our own simple version
data ValidationOutcome a
  = Ok a
  | Error ValidationErrors
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
    (Error a, Error b) -> Error (a <> b)
  {-# INLINE (<!>) #-}

instance F.Foldable ValidationOutcome where
  foldr f x (Ok a) = f a x
  foldr _ x (Error _) = x
  {-# INLINE foldr #-}

instance T.Traversable ValidationOutcome where
  traverse f x = case x of
    (Ok a) -> Ok <$> f a
    (Error err) -> pure (Error err)
  {-# INLINE traverse #-}

newtype ValidationErrors = ValidationErrors { getValidatorErrors :: [Text] }
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via [Text]


validate :: Sc.JSONSchema -> JSON.Value -> ValidationOutcome JSON.Value
validate schema val = traverse (validate' val) (Sc.sValidators $ Sc.schema schema) $> val

validate' :: JSON.Value -> Sc.Validator -> ValidationOutcome ()
validate' value = \case
  Sc.ValType valType -> validateType value valType
  Sc.ValObject valObj -> validateObject value valObj
  Sc.ValBool b -> validateBoolean b
  Sc.ValArray valA -> validateArray value valA
  Sc.ValNumeric valN -> validateNumeric value valN


validateType :: JSON.Value -> Sc.TypeValidator -> ValidationOutcome ()
validateType value valType =
  let acceptableTypes = case valType of
        Sc.OneType t -> V.singleton t
        Sc.MultipleTypes typs -> typs
      want t = if t `V.elem` acceptableTypes
        then Ok ()
        else Error $ mkError acceptableTypes t
  in case value of
      JSON.Object _ -> want Sc.PTObject
      JSON.Array _  -> want Sc.PTArray
      JSON.String _ -> want Sc.PTString
      JSON.Bool _   -> want Sc.PTBoolean
      JSON.Null     -> want Sc.PTNull
      JSON.Number n -> if Scientific.isInteger n
        then want Sc.PTInteger <!> want Sc.PTNumber
        else want Sc.PTNumber

  where
    mkError :: V.Vector Sc.PrimitiveType -> Sc.PrimitiveType -> ValidationErrors
    mkError typs t =
      let expected = if V.length typs == 1
            then "type: " <> Sc.prettyPrimitiveType (V.head typs)
            else "one of the following type: " <> Tx.intercalate ", " (F.toList $ fmap Sc.prettyPrimitiveType typs)
       in ValidationErrors ["Expected " <> expected <> " but got " <> Sc.prettyPrimitiveType t]


validateObject :: JSON.Value -> Sc.ObjectValidator -> ValidationOutcome ()
validateObject value valObj = case value of
  JSON.Object o -> F.traverse_ id
    [ void $ Map.traverseWithKey (validateProps (Sc.ovProperties valObj)) o
    , void $ validateAdditionalProps o valObj
    , void $ validatePatternProps o (Sc.ovPatternProps valObj)
    ]
  _ -> pure ()

  where
    validateProps props key jsonVal = case Map.lookup key props of
      Nothing -> pure ()
      Just schema -> void $ validate (Sc.SubSchema schema) jsonVal

    validateAdditionalProps o valObj =
      let additionalKeyVals = getAdditionalKeyValues o valObj
      in
      case Sc.ovAdditionalProps valObj of
        Sc.AllAdditionalProperties -> pure ()
        Sc.NoAdditionalProperties -> if null additionalKeyVals
          then pure ()
          else Error $ ValidationErrors ["Unexpected keys: " <> Tx.pack (show $ map fst additionalKeyVals)]
        Sc.SomeAdditionalProperties schema ->
          F.traverse_ (validate (Sc.SubSchema schema) . snd) additionalKeyVals

    validatePatternProps o patternProps = F.traverse_ (validateOnePatternProp o) (OrdMap.toList patternProps)

    validateOnePatternProp o (regexp, schema) = void $ Map.traverseWithKey
      (\k v -> if k =~ regexp then void (validate (Sc.SubSchema schema) v) else pure ())
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

validateBoolean :: Bool -> ValidationOutcome ()
validateBoolean b = if b
  then pure ()
  else Error $ ValidationErrors ["Boolean schema is false"]

validateArray :: JSON.Value -> Sc.ArrayValidator -> ValidationOutcome ()
validateArray value valA = case value of
  JSON.Array arr -> F.traverse_ id
    [ validateMaxItems arr valA
    , validateMinItems arr valA
    , validateItems arr valA
    , validateUnique arr valA
    ]
  _ -> pure ()

  where
    validateMaxItems a v = case Sc.avMaxItems v of
      Nothing -> Ok ()
      Just x -> if V.length a <= x
        then Ok ()
        else Error $ ValidationErrors
          [Tx.pack $ "MaxItems is " <> show x <> " but array has " <> show (V.length a)]

    validateMinItems a v = case Sc.avMinItems v of
      Nothing -> Ok ()
      Just x -> if V.length a >= x
        then Ok ()
        else Error $ ValidationErrors
          [Tx.pack $ "MinItems is " <> show x <> " but array has " <> show (V.length a)]

    validateItems a v = case Sc.avItems v of
      Sc.SingleSchema schema -> F.traverse_ (validate (Sc.SubSchema schema)) a

      Sc.MultipleSchemas schemas -> if V.length a <= V.length schemas
        then
          F.traverse_ (\(schema, val) -> validate (Sc.SubSchema schema) val) (V.zip schemas a)
        else
          let remainingItems = V.drop (V.length schemas) a
           in case Sc.avAdditionalItems v of
                Sc.AdditionalAllAllowed
                  -> pure ()

                Sc.AdditionalAllForbidden
                  -> Error $ ValidationErrors ["Too many items"]

                Sc.AdditionalSingleSchema schema
                  -> F.traverse_ (validate (Sc.SubSchema schema)) remainingItems

                Sc.AdditionalMultipleSchemas schemas'
                  -> if V.length remainingItems <= V.length schemas'
                    then
                      F.traverse_ (\(s, val) -> validate (Sc.SubSchema s) val) (V.zip schemas' remainingItems)
                    else
                      Error $ ValidationErrors ["Too many items"]

      Sc.NoItemsValidator -> pure ()

    validateUnique a v = case Sc.avUniqueItems v of
      Sc.ItemsCanBeDuplicated -> pure ()
      Sc.ItemsMustBeUnique -> if length (Set.fromList $ V.toList a) == length a
        then pure ()
        else Error $ ValidationErrors ["Items aren't unique"]

validateNumeric :: JSON.Value -> Sc.NumericValidator -> ValidationOutcome ()
validateNumeric value valN = case value of
  JSON.Number n -> F.traverse_ id
    [ validateMultipleOf n (Sc.nvMultipleOf valN)
    , validateMinimum n (Sc.nvMinimum valN)
    , validateMaximum n (Sc.nvMaximum valN)
    , validateExclusiveMinimum n (Sc.nvExclusiveMinimum valN)
    , validateExclusiveMaximum n (Sc.nvExclusiveMaximum valN)
    ]
  _ -> pure ()

  where
    validateMultipleOf n = \case
      Nothing -> pure ()
      Just x ->
        let (coeffN, baseN) = (Scientific.coefficient n, Scientific.base10Exponent n)
            (coeffX, baseX) = (Scientific.coefficient x, Scientific.base10Exponent x)
         in if baseN == baseX
              then assert "multipleOf" (coeffN `mod` coeffX == 0)
              else
                let diff = abs (baseN - baseX)
                    (cn', cx') = if baseN > baseX
                      then (coeffN * 10 ^ diff, coeffX)
                      else (coeffN, coeffX * 10 ^ diff)
                 in assert "multipleOf" (cn' `mod` cx' == 0)

    validateMinimum n = maybe (pure ()) (\x -> assert "minimum" (n >= x))
    validateMaximum n = maybe (pure ()) (\x -> assert "maximum" (n <= x))
    validateExclusiveMinimum n = maybe (pure ()) (\x -> assert "exclusiveMinimum" (x < n))
    validateExclusiveMaximum n = maybe (pure ()) (\x -> assert "exclusiveMaximum" (x > n))

    assert :: Text -> Bool -> ValidationOutcome ()
    assert keyword = \case
      True -> Ok ()
      False -> Error $ ValidationErrors [keyword <> " failed"]

    -- whenJust f = \case
    --   Nothing -> pure ()
    --   Just x -> f 


  -- { nvMultipleOf       :: Maybe Scientific
  -- , nvMinimum          :: Maybe Scientific
  -- , nvMaximum          :: Maybe Scientific
  -- , nvExclusiveMinimum :: Maybe Scientific
  -- , nvExclusiveMaximum :: Maybe Scientific
  -- }
