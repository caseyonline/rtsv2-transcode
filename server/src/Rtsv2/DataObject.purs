module Rtsv2.DataObject
       ( MessageDestination(..)
       , Message(..)
       , SubscriberId
       , ObjectUpdateOperation(..)
       , ObjectUpdateMessage(..)
       , ObjectUpdateResponseMessage(..)
       , ObjectUpdateResponse(..)
       , ObjectUpdateError(..)
       , ObjectKey
       , ObjectValue(..)
       , Object
       , ObjectBroadcastMessage(..)
       , startLink
       , shouldProcessMessage
       , class DataObjectRef
       , ref
       , new
       , update
       ) where

import Prelude

import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Utils (Ref, systemTimeMs)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Names as Names
import Shared.Common (Milliseconds(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype SubscriberId = SubscriberId String

data MessageDestination = Publisher
                        | Broadcast
                        | Private (List SubscriberId)

newtype Message = Message { sender :: SubscriberId
                          , destination :: MessageDestination
                          , msg :: String
                          , ref :: Ref
                          }

newtype ObjectKey = ObjectKey String

data ObjectValue = Int Int
                 | String String
                 | List ObjectValue
                 | Map { key :: ObjectKey
                       , value :: ObjectValue }

newtype Object = Object { keyValues :: Map String ObjectValue
                        , version :: Int
                        }

data ObjectUpdateOperation = Inc { key :: ObjectKey
                                 , increment :: Int
                                 , initialValue :: Maybe Int
                                 }
                           | Dec { key :: ObjectKey
                                 , decrement :: Int
                                 , initialValue :: Maybe Int
                                 }
                           | CompareAndSwap { key :: ObjectKey
                                            , compare :: ObjectValue
                                            , swap :: ObjectValue
                                            , createIfMissing :: Boolean
                                            }
                           | Update { key :: ObjectKey
                                    , value :: ObjectValue
                                    , createIfMissing :: Boolean
                                    }
                           | Insert { key :: ObjectKey
                                    , value :: ObjectValue
                                    , createIfMissing :: Boolean
                                    }
                           | Remove { key :: ObjectKey
                                    , value :: ObjectValue
                                    , failIfMissing :: Boolean
                                    }

newtype ObjectUpdateMessage = ObjectUpdateMessage { sender :: SubscriberId
                                                  , senderRef :: String
                                                  , operation :: ObjectUpdateOperation
                                                  , ref :: Ref }

data ObjectUpdateError = InvalidKey ObjectKey
                       | InvalidOperation ObjectKey

data ObjectUpdateResponse = Ok
                          | ObjectUpdateError

newtype ObjectUpdateResponseMessage = ObjectUpdateResponseMessage { to :: SubscriberId
                                                                  , senderRef :: String
                                                                  , response :: ObjectUpdateResponse
                                                                  , ref :: Ref }

newtype ObjectBroadcastMessage = ObjectBroadcastMessage { object :: Object
                                                        , ref :: Ref }

class DataObjectRef a where
  ref :: a -> Ref

type State key =
  { expireAfter :: Milliseconds
  , refs :: Map { key :: key
                , ref :: Ref} Milliseconds
  }

data Msg = DoExpiry

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: Unit -> Effect StartLinkResult
startLink _ = Gen.startLink serverName init handleInfo

shouldProcessMessage :: forall key msg. Ord key => DataObjectRef msg => key -> msg -> Effect Boolean
shouldProcessMessage key msg = Gen.doCall serverName
  (\state@{refs} ->
    let
      msgRef = ref msg
    in
      case Map.member {key, ref: msgRef} refs of
        true -> pure $ CallReply false state
        false -> do
          now <- systemTimeMs
          pure $ CallReply true state{refs = Map.insert {key, ref: msgRef} now refs}
  )

new :: Object
new = Object { keyValues: Map.empty
             , version: 0 }

update :: ObjectUpdateOperation -> Object -> Either ObjectUpdateError Object
update updateMessage currentObject =
  Right currentObject

------------------------------------------------------------------------------
-- gen_server callbacks
------------------------------------------------------------------------------
init :: forall key. Effect (State key)
init = do
  let
    expireAfter = Milliseconds 1000.0
    expireIntervalMs = 1000
  _ <- Timer.sendEvery serverName expireIntervalMs DoExpiry
  pure { expireAfter
       , refs: Map.empty }

handleInfo :: forall key. Msg -> State key -> Effect (CastResult (State key))
handleInfo msg state =
  case msg of
    DoExpiry -> do
      state2 <- doExpiry state
      pure $ CastNoReply state

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------
serverName :: forall key. ServerName (State key) Msg
serverName = Names.dataObjectName

doExpiry :: forall key. State key -> Effect (State key)
doExpiry state@{expireAfter, refs} = do
  now <- systemTimeMs
  let
    expireTime = now - expireAfter
    refs2 = foldlWithIndex (\key acc timestamp ->
                             if timestamp < expireTime then acc
                             else Map.insert key timestamp acc
                           )
            Map.empty
            refs
  pure state{refs = refs2}

------------------------------------------------------------------------------
-- SubscriberId
derive newtype instance readForeignSubscriberId :: ReadForeign SubscriberId
derive newtype instance writeForeignSubscriberId :: WriteForeign SubscriberId

------------------------------------------------------------------------------
-- MessageDestination
derive instance genericMessageDestination :: Generic MessageDestination _

instance readForeignMessageDestination :: ReadForeign MessageDestination where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignMessageDestination :: WriteForeign MessageDestination where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- Message
instance dataObjectRefMessage :: DataObjectRef Message where
  ref (Message {ref: msgRef}) = msgRef
derive newtype instance readForeignMessage :: ReadForeign Message
derive newtype instance writeForeignMessage :: WriteForeign Message

------------------------------------------------------------------------------
-- ObjectKey
derive newtype instance readForeignObjectKey :: ReadForeign ObjectKey
derive newtype instance writeForeignObjectKey :: WriteForeign ObjectKey

------------------------------------------------------------------------------
-- Object
derive newtype instance readForeignObject :: ReadForeign Object
derive newtype instance writeForeignObject :: WriteForeign Object

------------------------------------------------------------------------------
-- ObjectValue
derive instance genericObjectValue :: Generic ObjectValue _

instance readForeignObjectValue :: ReadForeign ObjectValue where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignObjectValue :: WriteForeign ObjectValue where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- ObjectUpdateOperation
derive instance genericObjectUpdateOperation :: Generic ObjectUpdateOperation _

instance readForeignObjectUpdateOperation :: ReadForeign ObjectUpdateOperation where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignObjectUpdateOperation :: WriteForeign ObjectUpdateOperation where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- ObjectUpdateMessage
instance dataObjectRefObjectUpdateMessage :: DataObjectRef ObjectUpdateMessage where
  ref (ObjectUpdateMessage {ref: msgRef}) = msgRef
derive newtype instance readForeignObjectUpdateMessage :: ReadForeign ObjectUpdateMessage
derive newtype instance writeForeignObjectUpdateMessage :: WriteForeign ObjectUpdateMessage

------------------------------------------------------------------------------
-- ObjectUpdateResponse
derive instance genericObjectUpdateResponse :: Generic ObjectUpdateResponse _

instance readForeignObjectUpdateResponse :: ReadForeign ObjectUpdateResponse where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignObjectUpdateResponse :: WriteForeign ObjectUpdateResponse where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- ObjectUpdateResponseMessage
instance dataObjectRefObjectUpdateResponseMessage :: DataObjectRef ObjectUpdateResponseMessage where
  ref (ObjectUpdateResponseMessage {ref: msgRef}) = msgRef
derive newtype instance readForeignObjectUpdateResponseMessage :: ReadForeign ObjectUpdateResponseMessage
derive newtype instance writeForeignObjectUpdateResponseMessage :: WriteForeign ObjectUpdateResponseMessage

------------------------------------------------------------------------------
-- ObjectBroadcastMessage
instance dataObjectRefObjectBroadcastMessage :: DataObjectRef ObjectBroadcastMessage where
  ref (ObjectBroadcastMessage {ref: msgRef}) = msgRef
derive newtype instance readForeignObjectBroadcastMessage :: ReadForeign ObjectBroadcastMessage
derive newtype instance writeForeignObjectBroadcastMessage :: WriteForeign ObjectBroadcastMessage
