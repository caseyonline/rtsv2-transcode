module Shared.Stream
  ( StreamId(..)
  , StreamVariant(..)
  , StreamAndVariant(..)
  , toStreamId
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Except (except)
import Data.Array ((!!))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), split)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype StreamId = StreamId String
newtype StreamVariant = StreamVariant String
data StreamAndVariant = StreamAndVariant StreamId StreamVariant

derive instance genericStreamId :: Generic StreamId _

derive instance newtypeStreamId :: Newtype StreamId _

instance eqStreamId :: Eq StreamId where
  eq = genericEq

instance compareStreamId :: Ord StreamId where
  compare = genericCompare

instance showStreamId :: Show StreamId where
  show = genericShow


derive instance genericStreamVariant :: Generic StreamVariant _

derive instance newtypeStreamVariant :: Newtype StreamVariant _

instance eqStreamVariant :: Eq StreamVariant where
  eq = genericEq

instance compareStreamVariant :: Ord StreamVariant where
  compare = genericCompare

instance showStreamVariant :: Show StreamVariant where
  show = genericShow
  

derive instance genericStreamAndVariant :: Generic StreamAndVariant _

instance eqStreamAndVariant :: Eq StreamAndVariant where
  eq = genericEq

instance compareStreamAndVariant :: Ord StreamAndVariant where
  compare = genericCompare

instance showStreamAndVariant :: Show StreamAndVariant where
  show = genericShow

instance readForeignStreamAndVariant :: ReadForeign StreamAndVariant where
  readImpl fgn = do
                 x <- readString fgn
                 let
                   y = split (Pattern ":") x
                   f = y !! 0 <#> StreamId
                   s = y !! 1 <#> StreamVariant
                   result = lift2 StreamAndVariant f s
                 except $ note (singleton (ForeignError "Failed to parse")) result

instance writeForeignStreamAndVariant :: WriteForeign StreamAndVariant where
  writeImpl (StreamAndVariant streamId streamVariant) = unsafeToForeign $ (unwrap streamId) <> ":" <> (unwrap streamVariant)
  
toStreamId :: StreamAndVariant -> StreamId
toStreamId (StreamAndVariant s _) = s
