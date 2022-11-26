module Classless.DecodeJson
  ( DecodeJson
  , array
  , boolean
  , class GDecodeJson
  , gDecodeJson
  , class Record
  , either
  , int
  , record
  , string
  , tuple
  , class DecodeJsonField
  , decodeJsonField
  , module Exp
  , maybe
  , number
  ) where

import Classless as Cls
import Classless.DecodeJson.Generic (class Sum, sum) as Exp
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Decoders as Arg
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Foreign.Object as FO
import Prelude (bind, ($), (<$>))
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

type DecodeJson a = Json -> Either JsonDecodeError a

int :: DecodeJson Int
int = Arg.decodeInt

string :: DecodeJson String
string = Arg.decodeString

number :: DecodeJson Number
number = Arg.decodeNumber

boolean :: DecodeJson Boolean
boolean = Arg.decodeBoolean

array :: forall a. DecodeJson a -> DecodeJson (Array a)
array = Arg.decodeArray

maybe :: forall a. DecodeJson a -> DecodeJson (Maybe a)
maybe = Arg.decodeMaybe

either :: forall a b. DecodeJson a -> DecodeJson b -> DecodeJson (Either a b)
either = Arg.decodeEither

tuple :: forall a b. DecodeJson a -> DecodeJson b -> DecodeJson (Tuple a b)
tuple = Arg.decodeTuple

class Record spec r | r -> spec where
  record :: { | spec } -> Json -> Either JsonDecodeError { | r }

instance
  ( GDecodeJson spec row list
  , RL.RowToList row list
  ) =>
  Record spec row where
  record spec json =
    case toObject json of
      Just object -> gDecodeJson spec object (Proxy :: Proxy list)
      Nothing -> Left $ TypeMismatch "Object"

class GDecodeJson (spec :: Row Type) (row :: Row Type) (list :: RL.RowList Type) | list -> row spec where
  gDecodeJson :: forall proxy. { | spec } -> FO.Object Json -> proxy list -> Either JsonDecodeError (Record row)

instance GDecodeJson () () RL.Nil where
  gDecodeJson _ _ _ = Right {}

instance
  ( DecodeJsonField value
  , GDecodeJson specX rowTail tail
  , IsSymbol field
  , Row.Cons field value rowTail row
  , Row.Lacks field rowTail
  , Row.Cons field (Json -> Either JsonDecodeError value) specX spec
  , Union specX x spec
  ) =>
  GDecodeJson spec row (RL.Cons field value tail) where
  gDecodeJson spec object _ = do
    let
      fieldName = reflectSymbol _field
      fieldValue = FO.lookup fieldName object

    case decodeJsonField decodeJson fieldValue of
      Just fieldVal -> do
        val <- lmap (AtKey fieldName) fieldVal
        rest <- gDecodeJson (Cls.pick spec :: { | specX }) object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

      Nothing ->
        Left $ AtKey fieldName MissingValue
    where
    _field = Proxy :: Proxy field
    decodeJson = Record.get _field spec

class DecodeJsonField a where
  decodeJsonField :: (Json -> Either JsonDecodeError a) -> Maybe Json -> Maybe (Either JsonDecodeError a)

instance
  DecodeJsonField (Maybe a) where
  decodeJsonField _ Nothing = Just $ Right Nothing
  decodeJsonField decodeJson (Just j) = Just $ decodeJson j

else instance
  DecodeJsonField a where
  decodeJsonField decodeJson j = decodeJson <$> j