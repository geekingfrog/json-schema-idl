{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.JSON.Validation where

import qualified Data.HashMap.Strict as Map
import qualified Data.Typeable as Typeable
import Data.Functor (($>), void)
import qualified Data.Scientific as Scientific
import qualified Data.Vector as V
import qualified Data.Aeson as JSON
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Functor.Alt
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Set as Set

import qualified Data.JSON.Schema as Sc

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

newtype ValidationErrors = ValidationErrors { getValidationErrors :: [Text] }
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via [Text]


validate :: Sc.JSONSchema -> JSON.Value -> ValidationOutcome JSON.Value
validate schema val = traverse (validate' val) (Sc.sValidations $ Sc.schema schema) $> val

validate' :: JSON.Value -> Sc.Validation -> ValidationOutcome ()
validate' value = \case
  Sc.ValType valType -> validateType value valType
  Sc.ValProperties valProp -> validateProperties value valProp
  Sc.ValBool b -> validateBoolean b


validateType :: JSON.Value -> Sc.ValidationType -> ValidationOutcome ()
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


validateProperties :: JSON.Value -> Sc.ValidationProperties -> ValidationOutcome ()
validateProperties value valProp = case value of
  JSON.Object o -> F.traverse_ id
    [ void $ Map.traverseWithKey (validateProps (Sc.vpProperties valProp)) o
    , void $ validateAdditionalProps o valProp
    ]
  _ -> pure ()

  where
    validateProps props key jsonVal = case Map.lookup key props of
      Nothing -> pure ()
      Just schema -> void $ validate (Sc.SubSchema schema) jsonVal

    validateAdditionalProps o valProp = case Sc.vpAdditionalProps valProp of
      Sc.AllAdditionalProperties -> pure ()
      Sc.NoAdditionalProperties ->
        let keys = Set.fromList (Map.keys o)
            allowedKeys = Set.fromList (Map.keys $ Sc.vpProperties valProp)
            diff = Set.difference allowedKeys keys
         in if Set.null diff
              then pure ()
              else Error $ ValidationErrors ["Unexpected keys: " <> Tx.pack (show diff)]
      _ -> pure ()

validateBoolean :: Bool -> ValidationOutcome ()
validateBoolean b = if b
  then pure ()
  else Error $ ValidationErrors ["Boolean schema is false"]
