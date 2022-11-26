module Classless.DecodeJson.Generic
  ( class DecodeRep
  , class DecodeRepArgs
  , decodeRepWith
  , decodeRepArgs
  , sum
  , class Sum
  ) where

import Prelude

import Classless (type (~), NoArgs, (~))
import Classless as Cls
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, fromString, toArray, toObject, toString, fromArray)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (Constructor)
import Data.Generic.Rep as Rep
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

type Encoding =
  { tagKey :: String
  , valuesKey :: String
  , unwrapSingleArguments :: Boolean
  }

defaultEncoding :: Encoding
defaultEncoding =
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  }

class Sum spec a | a -> spec where
  sum :: { | spec } -> Json -> Either JsonDecodeError a

instance (Rep.Generic a r, DecodeRep spec r) => Sum spec a where
  sum spec = sumWith spec defaultEncoding

class DecodeRep sumSpec r | r -> sumSpec where
  decodeRepWith :: { | sumSpec } -> Encoding -> Json -> Either JsonDecodeError r

instance DecodeRep () Rep.NoConstructors where
  decodeRepWith _ _ _ = Left $ UnexpectedValue $ fromString "NoConstructors (Cannot decode empty data type)"

instance
  ( DecodeRep specA a
  , DecodeRep specB b
  , TypeEquals a (Constructor sym x)
  , Cons sym s specB spec
  , Cons sym s () specA
  , Lacks sym specB
  , IsSymbol sym
  , Union specA specB spec
  , Union specB specA spec
  ) =>
  DecodeRep spec (Rep.Sum a b) where
  decodeRepWith spec e j =
    Rep.Inl
      <$> decodeRepWith specA e j <|> Rep.Inr <$> decodeRepWith specB e j
    where
    specA = Cls.pick spec :: { | specA }
    specB = Cls.pick spec :: { | specB }

withTag
  :: Encoding
  -> Json
  -> String
  -> Either JsonDecodeError
       { tag :: String
       , decodingErr :: JsonDecodeError -> JsonDecodeError
       }
withTag e j name = do
  let decodingErr = Named name
  jObj <- note (decodingErr $ TypeMismatch "Object") (toObject j)
  jTag <- note (decodingErr $ AtKey e.tagKey MissingValue) (FO.lookup e.tagKey jObj)
  tag <- note (decodingErr $ AtKey e.tagKey $ TypeMismatch "String") (toString jTag)
  when (tag /= name)
    $ Left
    $ decodingErr
    $ AtKey e.tagKey
    $ UnexpectedValue
    $ fromString tag
  pure { tag, decodingErr }

withTagAndValues
  :: Encoding
  -> Json
  -> String
  -> Either JsonDecodeError
       { tag :: String
       , values :: Json
       , decodingErr :: JsonDecodeError -> JsonDecodeError
       }
withTagAndValues e j name = do
  { tag, decodingErr } <- withTag e j name
  jObj <- note (decodingErr $ TypeMismatch "Object") (toObject j)
  values <- note (decodingErr $ AtKey e.valuesKey MissingValue) (FO.lookup e.valuesKey jObj)
  pure { tag, values, decodingErr }

construct
  :: forall spec e t s
   . DecodeRepArgs spec t
  => spec
  -> Encoding
  -> Array Json
  -> (JsonDecodeError -> e)
  -> Either e (Rep.Constructor s t)
construct spec e valuesArray decodingErr = do
  { init, rest } <- lmap decodingErr $ decodeRepArgs spec valuesArray
  when (rest /= [])
    $ Left
    $ decodingErr
    $ AtKey e.valuesKey
    $ UnexpectedValue (fromArray rest)
  pure $ Rep.Constructor init

instance
  ( IsSymbol name
  , Cons name NoArgs () spec
  ) =>
  DecodeRep
    spec
    (Rep.Constructor name Rep.NoArguments) where
  decodeRepWith _ e j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    { decodingErr } <- withTag e j name
    construct unit e [] decodingErr

else instance
  ( IsSymbol name
  , Cons name (Json -> Either JsonDecodeError a) () spec
  ) =>
  DecodeRep
    spec
    (Rep.Constructor name (Rep.Argument a)) where
  decodeRepWith spec e j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    { values, decodingErr } <- withTagAndValues e j name
    if e.unwrapSingleArguments then construct specA e [ values ] decodingErr
    else do
      valuesArray <- note (decodingErr $ AtKey e.valuesKey $ TypeMismatch "Array") (toArray values)
      construct specA e valuesArray decodingErr
    where
    specA = Record.get (Proxy :: _ name) spec

else instance
  ( IsSymbol name
  , DecodeRepArgs specA a
  , Cons name specA () spec
  ) =>
  DecodeRep
    spec
    (Rep.Constructor name a) where
  decodeRepWith spec e j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    { values, decodingErr } <- withTagAndValues e j name
    valuesArray <- note (decodingErr $ AtKey e.valuesKey $ TypeMismatch "Array") (toArray values)
    construct (specA) e valuesArray decodingErr
    where
    specA = Record.get (Proxy :: _ name) spec

class DecodeRepArgs spec r | r -> spec where
  decodeRepArgs :: spec -> Array Json -> Either JsonDecodeError { init :: r, rest :: Array Json }

instance DecodeRepArgs Unit Rep.NoArguments where
  decodeRepArgs _ js = Right { init: Rep.NoArguments, rest: js }

instance
  ( DecodeRepArgs specA a
  , DecodeRepArgs specB b
  ) =>
  DecodeRepArgs
    (specA ~ specB)
    (Rep.Product a b) where
  decodeRepArgs (specA ~ specB) js = do
    { init: a, rest: js' } <- decodeRepArgs specA js
    { init: b, rest: js'' } <- decodeRepArgs specB js'
    pure { init: Rep.Product a b, rest: js'' }

instance DecodeRepArgs (Json -> Either JsonDecodeError a) (Rep.Argument a) where
  decodeRepArgs decodeJsonSpec js = do
    { head, tail } <- note (TypeMismatch "NonEmptyArray") (uncons js)
    { init: _, rest: tail } <<< Rep.Argument <$> decodeJsonSpec head

sumWith :: forall a spec r. Rep.Generic a r => DecodeRep spec r => { | spec } -> Encoding -> Json -> Either JsonDecodeError a
sumWith spec e = map Rep.to <<< decodeRepWith spec e

class DecodeLiteral r where
  decodeLiteral :: (String -> String) -> Json -> Either JsonDecodeError r

instance (DecodeLiteral a, DecodeLiteral b) => DecodeLiteral (Rep.Sum a b) where
  decodeLiteral tagNameTransform j = Rep.Inl <$> decodeLiteral tagNameTransform j <|> Rep.Inr <$> decodeLiteral tagNameTransform j

instance (IsSymbol name) => DecodeLiteral (Rep.Constructor name (Rep.NoArguments)) where
  decodeLiteral tagNameTransform j = do
    let name = reflectSymbol (Proxy :: Proxy name)
    let decodingErr = Named name
    tag <- note (decodingErr $ TypeMismatch "String") (toString j)
    when (tag /= tagNameTransform name)
      $ Left
      $ decodingErr
      $ UnexpectedValue (fromString tag)
    pure $ Rep.Constructor (Rep.NoArguments)

type FailMessage =
  Text "`decodeLiteralSum` can only be used with sum types, where all of the constructors are nullary. This is because a string literal cannot be encoded into a product type."

instance
  Fail FailMessage =>
  DecodeLiteral (Rep.Product a b) where
  decodeLiteral _ _ = unsafeCrashWith "unreachable DecodeLiteral was reached."
