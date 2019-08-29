{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSON.Validation where

import           Text.Regex.PCRE.Heavy ((=~))
import qualified Data.Aeson          as JSON
import qualified Data.Foldable       as F
import           Data.Functor        (void, ($>))
import           Data.Functor.Alt
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict     as OrdMap
import qualified Data.Scientific     as Scientific
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Tx
import qualified Data.Traversable    as T
import qualified Data.Typeable       as Typeable
import qualified Data.Vector         as V

import qualified Data.JSON.Schema    as Sc

-- Data.Validation on hackage requires lens oÔ so let's roll our own simple version
data ValidationOutcome a
  = Ok a
  | Error ValidatorErrors
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

newtype ValidatorErrors = ValidatorErrors { getValidatorErrors :: [Text] }
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
    mkError :: V.Vector Sc.PrimitiveType -> Sc.PrimitiveType -> ValidatorErrors
    mkError typs t =
      let expected = if V.length typs == 1
            then "type: " <> Sc.prettyPrimitiveType (V.head typs)
            else "one of the following type: " <> Tx.intercalate ", " (F.toList $ fmap Sc.prettyPrimitiveType typs)
       in ValidatorErrors ["Expected " <> expected <> " but got " <> Sc.prettyPrimitiveType t]


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

    validateAdditionalProps o valObj = case Sc.ovAdditionalProps valObj of
      Sc.AllAdditionalProperties -> pure ()
      Sc.NoAdditionalProperties ->
        let keys = Set.fromList (Map.keys o)
            allowedKeys = Set.fromList (Map.keys $ Sc.ovProperties valObj)
            diff = Set.difference allowedKeys keys
         in if Set.null diff
              then pure ()
              else Error $ ValidatorErrors ["Unexpected keys: " <> Tx.pack (show diff)]
      _ -> pure ()

    validatePatternProps o patternProps = F.traverse_ (validateOnePatternProp o) (OrdMap.toList patternProps)

    validateOnePatternProp o (regexp, schema) = void $ Map.traverseWithKey
      (\k v -> if k =~ regexp then void (validate (Sc.SubSchema schema) v) else pure ())
      o

validateBoolean :: Bool -> ValidationOutcome ()
validateBoolean b = if b
  then pure ()
  else Error $ ValidatorErrors ["Boolean schema is false"]

validateArray :: JSON.Value -> Sc.ArrayValidator -> ValidationOutcome ()
validateArray value valA = case value of
  JSON.Array arr -> F.traverse_ id
    [ void $ validateMaxItems arr valA
    , void $ validateMinItems arr valA
    ]
  _ -> pure ()

  where
    validateMaxItems a v = case Sc.avMaxItems v of
      Nothing -> Ok ()
      Just x -> if V.length a <= x
        then Ok ()
        else Error $ ValidatorErrors
          [Tx.pack $ "MaxItems is " <> show x <> " but array has " <> show (V.length a)]

    validateMinItems a v = case Sc.avMinItems v of
      Nothing -> Ok ()
      Just x -> if V.length a >= x
        then Ok ()
        else Error $ ValidatorErrors
          [Tx.pack $ "MinItems is " <> show x <> " but array has " <> show (V.length a)]
