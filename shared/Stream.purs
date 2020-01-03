module Shared.Stream
  ( StreamId(..)
  , StreamVariantId(..)
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
import Data.List (List)
import Data.List.NonEmpty (singleton)
import Data.String (Pattern(..), split)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

data StreamId = StreamId String
data StreamVariantId = StreamVariantId String String

derive instance genericStreamId :: Generic StreamId _

instance eqStreamId :: Eq StreamId where
  eq = genericEq

instance compareStreamId :: Ord StreamId where
  compare = genericCompare

instance showStreamId :: Show StreamId where
  show = genericShow

derive instance genericStreamVariantId :: Generic StreamVariantId _

instance eqStreamVariantId :: Eq StreamVariantId where
  eq = genericEq

instance compareStreamVariantId :: Ord StreamVariantId where
  compare = genericCompare

instance showStreamVariantId :: Show StreamVariantId where
  show = genericShow

instance readForeignStreamVariantId :: ReadForeign StreamVariantId where
  readImpl fgn = do
                 x <- readString fgn
                 let
                   y = split (Pattern ":") x
                   f = y !! 0
                   s = y !! 1
                   result = lift2 StreamVariantId f s
                 except $ note (singleton (ForeignError "Failed to parse")) result

instance writeForeignStreamVariantId :: WriteForeign StreamVariantId where
  writeImpl (StreamVariantId streamId streamVariantId) = unsafeToForeign $ streamId <> ":" <> streamVariantId
  
toStreamId :: StreamVariantId -> StreamId
toStreamId (StreamVariantId s _) = StreamId s
