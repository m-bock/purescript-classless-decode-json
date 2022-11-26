module Test.Classless.DecodeJson where

import Prelude

import Classless (class Init, class InitRecord, class InitSum, initRecord, initSum, noArgs, (~))
import Classless.DecodeJson (DecodeJson)
import Classless.DecodeJson as Dec
import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Record as Record

type Items = Array
  ( Either
      String
      (Tuple Int Boolean)
  )

decItems'1 :: DecodeJson Items
decItems'1 = Dec.array
  ( Dec.either
      Dec.string
      (Dec.tuple Dec.int Dec.boolean)
  )

type User =
  { name :: String
  , age :: Int
  , loggedIn :: Boolean
  , coordinates :: Array { x :: Int, y :: Int }
  }

decUser'1 :: DecodeJson User
decUser'1 = Dec.record
  { name: Dec.string
  , age: Dec.int
  , loggedIn: Dec.boolean
  , coordinates: Dec.array $ Dec.record
      { x: Dec.int
      , y: Dec.int
      }
  }

data RemoteData
  = NotAsked
  | Loading Int Int Int
  | Error String
  | Success
      { status :: Int
      , body :: String
      }

derive instance Generic RemoteData _

decRemoteData'1 :: DecodeJson RemoteData
decRemoteData'1 = Dec.sum
  { "NotAsked": noArgs
  , "Loading": Dec.int ~ Dec.int ~ Dec.int
  , "Error": Dec.string
  , "Success": Dec.record
      { status: Dec.int
      , body: Dec.string
      }
  }

class MyDecodeJson a where
  decodeJson :: Json -> Either JsonDecodeError a

instance MyDecodeJson Int where
  decodeJson = Dec.int

instance MyDecodeJson String where
  decodeJson = Dec.string

instance MyDecodeJson Boolean where
  decodeJson = Dec.boolean

instance (MyDecodeJson a) => MyDecodeJson (Array a) where
  decodeJson = Dec.array decodeJson

instance (MyDecodeJson a) => MyDecodeJson (Maybe a) where
  decodeJson = Dec.maybe decodeJson

instance (MyDecodeJson a, MyDecodeJson b) => MyDecodeJson (Either a b) where
  decodeJson = Dec.either decodeJson decodeJson

instance (MyDecodeJson a, MyDecodeJson b) => MyDecodeJson (Tuple a b) where
  decodeJson = Dec.tuple decodeJson decodeJson

data MyInit = MyInit

instance (MyDecodeJson a) => Init MyInit (DecodeJson a) where
  init _ = decodeJson

instance (Dec.Record r' r, InitRecord MyInit r') => MyDecodeJson (Record r) where
  decodeJson = Dec.record $ initRecord MyInit

genericSum :: forall r' a. Dec.Sum r' a => InitSum MyInit r' => DecodeJson a
genericSum = Dec.sum $ initSum MyInit

decItems'2 :: DecodeJson Items
decItems'2 = decodeJson

decUser'2 :: DecodeJson User
decUser'2 = decodeJson

decRemoteData'2 :: DecodeJson RemoteData
decRemoteData'2 = genericSum

-- decUser'3 :: DecodeJson User
-- decUser'3 = Dec.record
--   $ Record.union
--       { age: Dec.int
--       }
--   $ initRecord MyInit

-- decAB :: DecodeJson { a :: Int, b :: Char }
-- decAB = Dec.record
--   $ Record.union
--       { b: Dec.char
--       }
--   $ initRecord MyInit

-- decRemoteData'3 :: DecodeJson RemoteData
-- decRemoteData'3 = Dec.sum
--   $ Record.union
--       { "Error": Dec.string
--       }
--   $ initSum MyInit

-- decRemoteData'4 :: DecodeJson RemoteData
-- decRemoteData'4 = Dec.sum
--   $ Record.union
--       { "Loading": DecodeJson ~ DecodeJson ~ Dec.int
--       }
--   $ initSum MyInit

-- decRemoteData'5 :: DecodeJson RemoteData
-- decRemoteData'5 = Dec.sum
--   $ Record.union
--       { "Success": Dec.record
--           $ Record.union
--               { status: Dec.int
--               }
--           $ initRecord MyInit
--       }
--   $ initSum MyInit
