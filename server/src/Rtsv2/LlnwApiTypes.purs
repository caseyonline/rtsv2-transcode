-- Notes:
-- * Unique ID per slot (64 bit int?)
-- * What does 415 error indicate?
-- * is 'profiles' within SlotDetails guaranteed to be non-empty?

module Rtsv2.LlnwApiTypes
       ( HlsPushAuth
       , HlsPushSpec
       , HlsPushSpecFormat
       , SlotDetails
       , SlotProfile
       , SlotPublishAuthType(..)
       , StreamDetails
       , StreamPublishProtocol(..)
       , StreamPublish
       , StreamRole(..)
       , SlotPublishAuth(..)
       )
       where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Erl.Data.List (List)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (F, ForeignError(..), readString, unsafeToForeign)
import Record (rename)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)
import Type.Prelude (SProxy(..))

data StreamPublishProtocol = Rtmp
                           | WebRTC

type StreamPublish =
  { host :: String
  , protocol :: StreamPublishProtocol
  , shortname :: String
  , streamName :: String
  }

data SlotPublishAuthType = Adobe
                         | Llnw
                         | Query

newtype SlotPublishAuth = SlotPublishAuth { authType :: SlotPublishAuthType
                                          , username :: String
                                          , password :: String
                                          }

type SlotProfile =
  { name :: String
  , streamName :: String
  , bitrate :: Int
  }

type SlotDetails =
  { name :: String
  , publishAuth :: SlotPublishAuth
  , subscribeValidation :: Boolean
  , profiles :: List SlotProfile
  }

type HlsPushAuth =
  { username :: String
  , password :: String
  }

data HlsPushSpecFormat = Hls

type HlsPushSpec =
  { format :: HlsPushSpecFormat
  , baseUrl :: String
  , auth :: HlsPushAuth
  }

data StreamRole = Primary
                | Backup

type StreamDetails =
  { role :: StreamRole
  , slot :: SlotDetails
  , push :: List HlsPushSpec
  }

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------
derive instance genericStreamPublishProtocol :: Generic StreamPublishProtocol _

instance readForeignStreamPublishProtocol :: ReadForeign StreamPublishProtocol where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "rtmp" = pure Rtmp
      toType "webrtc" = pure WebRTC
      toType unknown = Nothing
      errorString s = "Unknown StreamPublishProtocol: " <> s

instance writeForeignStreamPublishProtocol :: WriteForeign StreamPublishProtocol where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Rtmp = "rtmp"
      toString WebRTC = "webrtc"

instance eqStreamPublishProtocol :: Eq StreamPublishProtocol where
  eq = genericEq

instance compareStreamPublishProtocol :: Ord StreamPublishProtocol where
  compare = genericCompare

instance showStreamPublishProtocol :: Show StreamPublishProtocol where
  show = genericShow

derive instance genericSlotPublishAuthType :: Generic SlotPublishAuthType _

instance readForeignSlotPublishAuthType :: ReadForeign SlotPublishAuthType where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "adobe" = pure Adobe
      toType "llnw" = pure Llnw
      toType "query" = pure Query
      toType unknown = Nothing
      errorString s = "Unknown SlotPublishAuthType: " <> s

instance writeForeignSlotPublishAuthType :: WriteForeign SlotPublishAuthType where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Adobe = "adobe"
      toString Llnw = "llnw"
      toString Query = "query"

instance eqSlotPublishAuthType :: Eq SlotPublishAuthType where
  eq = genericEq

instance compareSlotPublishAuthType :: Ord SlotPublishAuthType where
  compare = genericCompare

instance showSlotPublishAuthType :: Show SlotPublishAuthType where
  show = genericShow

instance readForeignSlotPublishAuth :: ReadForeign SlotPublishAuth where
  readImpl o = decode =<< readString o
    where
    decode :: String -> F SlotPublishAuth
    decode s = do
      parsed <- readJSON' s
      pure $ SlotPublishAuth $ rename
        (SProxy :: SProxy "type")
        (SProxy :: SProxy "authType")
        parsed

instance writeForeignSlotPublishAuth :: WriteForeign SlotPublishAuth where
  writeImpl = (unsafeToForeign <<< encode)
    where
      encode :: SlotPublishAuth -> String
      encode (SlotPublishAuth r) = do
        writeJSON $ rename
          (SProxy :: SProxy "authType")
          (SProxy :: SProxy "type")
          r

instance readForeignHlsPushSpecFormat :: ReadForeign HlsPushSpecFormat where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "hls" = pure Hls
      toType unknown = Nothing
      errorString s = "Unknown HlsPushSpecFormat: " <> s

instance writeForeignHlsPushSpecFormat :: WriteForeign HlsPushSpecFormat where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Hls = "hls"

instance readForeignStreamRole :: ReadForeign StreamRole where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "primary" = pure Primary
      toType "backup" = pure Backup
      toType unknown = Nothing
      errorString s = "Unknown StreamRole: " <> s

instance writeForeignStreamRole :: WriteForeign StreamRole where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Primary = "primary"
      toString Backup = "backup"


