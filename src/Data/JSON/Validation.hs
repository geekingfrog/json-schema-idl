{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.JSON.Validation where

import Data.Functor (($>))
import qualified Data.Scientific as Scientific
import qualified Data.Vector as V
import qualified Data.Aeson as JSON
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Functor.Alt
import Data.Text (Text)
import qualified Data.Text as Tx

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
