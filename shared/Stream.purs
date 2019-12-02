module Shared.Stream
  ( StreamId(..)
  , StreamVariantId(..)
  , toStreamId
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)

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


toStreamId :: StreamVariantId -> StreamId
toStreamId (StreamVariantId s _) = StreamId s
