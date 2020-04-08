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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Erl.Data.List (List, null, singleton, uncons, (:))
import Erl.Data.List as List
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

data ObjectValue = Bool Boolean
                 | Counter Number
                 | Number Number
                 | String String
                 | List (List ObjectValue)
                 | Map (Map ObjectKey ObjectValue)

newtype Object = Object { map :: Map ObjectKey ObjectValue
                        , version :: Int
                        }

data ObjectUpdateOperation = Inc { keys :: List ObjectKey
                                 , increment :: Number
                                 , createIfKeyMissing :: Boolean
                                 }
                           | Dec { keys :: List ObjectKey
                                 , decrement :: Number
                                 , createIfKeyMissing :: Boolean
                                 }
                           | CompareAndSwap { keys :: List ObjectKey
                                            , compare :: ObjectValue
                                            , swap :: ObjectValue
                                            , createIfKeyMissing :: Boolean
                                            }
                           | Add { keys :: List ObjectKey
                                 , value :: ObjectValue
                                 , failIfKeyPresent :: Boolean
                                 }
                           | Update { keys :: List ObjectKey
                                    , value :: ObjectValue
                                    , createIfKeyMissing :: Boolean
                                    }
                           | Delete { keys :: List ObjectKey
                                    , failIfKeyMissing :: Boolean
                                    }
                           | ListInsert { keys :: List ObjectKey
                                        , value :: ObjectValue
                                        , createIfKeyMissing :: Boolean
                                        , failIfValuePresent :: Boolean
                                        }
                           | ListRemove { keys :: List ObjectKey
                                        , value :: ObjectValue
                                        , failIfKeyMissing :: Boolean
                                        , failIfValueMissing :: Boolean
                                        }


newtype ObjectUpdateMessage = ObjectUpdateMessage { sender :: SubscriberId
                                                  , senderRef :: String
                                                  , operation :: ObjectUpdateOperation
                                                  , ref :: Ref }

data ObjectUpdateError = InvalidKey { keys :: List ObjectKey }
                       | InvalidValue { keys :: List ObjectKey
                                      , value :: ObjectValue }
                       | InvalidOperation { keys :: List ObjectKey }
                       | CompareAndSwapFailed { keys :: List ObjectKey
                                              , currentValue :: ObjectValue }
                       | InvalidRequest

data ObjectUpdateResponse = Ok
                          | Error ObjectUpdateError

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
new = Object { map: Map.empty
             , version: 0 }

update :: ObjectUpdateOperation -> Object -> Either ObjectUpdateError Object
update (Inc {keys, increment, createIfKeyMissing}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap createIfKeyMissing doInc
  where
    doInc (Just (Counter value)) = Right $ Just (Counter (value + increment))
    doInc Nothing = Right $ Just (Counter increment)
    doInc (Just _) = Left $ InvalidOperation {keys}

update (Dec {keys, decrement, createIfKeyMissing}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap createIfKeyMissing doDec
  where
    doDec (Just (Counter value)) = Right $ Just (Counter (value - decrement))
    doDec Nothing = Right $ Just (Counter (0.0 - decrement))
    doDec (Just _) = Left $ InvalidOperation {keys}

update (CompareAndSwap {keys, compare, swap, createIfKeyMissing}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap createIfKeyMissing doCas
  where
    doCas (Just currentValue) | currentValue /= compare = Left $ CompareAndSwapFailed {keys, currentValue}
    doCas _ = Right $ Just swap

update (Add {keys, value, failIfKeyPresent}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap true doAdd
  where
    doAdd (Just _) | failIfKeyPresent = Left $ InvalidKey {keys}
    doAdd _ = Right $ Just value

update (Update {keys, value, createIfKeyMissing}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap createIfKeyMissing doUpdate
  where
    doUpdate _ = Right $ Just value

update (Delete {keys, failIfKeyMissing}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap true doDelete
  where
    doDelete Nothing | failIfKeyMissing = Left $ InvalidKey {keys}
    doDelete _ = Right $ Nothing

update (ListInsert {keys, value, createIfKeyMissing, failIfValuePresent}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap createIfKeyMissing doListInsert
  where
    doListInsert Nothing = Right $ Just (List (singleton value))
    doListInsert (Just (List list)) | not failIfValuePresent = Right $ Just (List (value : list))
    doListInsert (Just (List list)) =
      if List.elemIndex value list == Nothing then Right $ Just (List (value : list))
      else Left $ InvalidValue {keys, value}
    doListInsert (Just _) = Left $ InvalidOperation {keys}

update (ListRemove {keys, value, failIfKeyMissing, failIfValueMissing}) (Object {map: currentMap, version}) =
  updateObject version <$> performUpdate keys currentMap true doListRemove
  where
    doListRemove Nothing | failIfValueMissing = Left $ InvalidKey {keys}
    doListRemove Nothing = Right $ Nothing
    doListRemove (Just (List list)) | not failIfValueMissing = Right $ Just $ List $ List.delete value list
    doListRemove (Just (List list)) =
      if List.elemIndex value list == Nothing then Left $ InvalidValue {keys, value}
      else Right $ Just $ List $ List.delete value list
    doListRemove (Just _) = Left $ InvalidOperation {keys}

-- TODO - map update / insert / remove


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

performUpdate :: List ObjectKey -> Map ObjectKey ObjectValue -> Boolean -> (Maybe ObjectValue -> Either ObjectUpdateError (Maybe ObjectValue)) -> Either ObjectUpdateError (Map ObjectKey ObjectValue)
performUpdate keys map createIfKeyMissing updateFun = performUpdate' keys keys map createIfKeyMissing updateFun

performUpdate' :: List ObjectKey -> List ObjectKey -> Map ObjectKey ObjectValue -> Boolean -> (Maybe ObjectValue -> Either ObjectUpdateError (Maybe ObjectValue)) -> Either ObjectUpdateError (Map ObjectKey ObjectValue)
performUpdate' fullPath keys map createIfKeyMissing updateFun =
  case uncons keys of
    Nothing -> Left $ InvalidKey {keys: fullPath}
    Just {head, tail} | null tail ->
      case Map.lookup head map of
        Nothing
          | createIfKeyMissing -> (\newValue -> Map.alter (\_ -> newValue) head map) <$> updateFun Nothing
          | otherwise -> Left $ InvalidKey {keys: fullPath}
        Just value ->
          (\newValue -> Map.alter (\_ -> newValue) head map) <$> updateFun (Just value)
    Just {head, tail} ->
      case Map.lookup head map of
        Nothing
          | createIfKeyMissing -> (\newMap -> Map.insert head (Map newMap) map) <$> performUpdate' fullPath tail Map.empty createIfKeyMissing updateFun
          | otherwise -> Left $ InvalidKey {keys: fullPath}
        Just (Map childMap) ->
          (\newMap -> Map.insert head (Map newMap) map) <$> performUpdate' fullPath tail childMap createIfKeyMissing updateFun
        _ -> Left $ InvalidKey {keys: fullPath}

updateObject :: Int -> Map ObjectKey ObjectValue -> Object
updateObject version newMap =
  Object {map: newMap, version: version + 1}

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
derive instance eqObjectKey :: Eq ObjectKey
derive instance newtypeObjectKey :: Newtype ObjectKey _
derive newtype instance readForeignObjectKey :: ReadForeign ObjectKey
derive newtype instance writeForeignObjectKey :: WriteForeign ObjectKey

------------------------------------------------------------------------------
-- Object
derive newtype instance readForeignObject :: ReadForeign Object
derive newtype instance writeForeignObject :: WriteForeign Object

------------------------------------------------------------------------------
-- ObjectValue
derive instance eqObjectValue :: Eq ObjectValue
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
-- ObjectUpdateError
derive instance eqObjectUpdateError :: Eq ObjectUpdateError
derive instance genericObjectUpdateError :: Generic ObjectUpdateError _

instance readForeignObjectUpdateError :: ReadForeign ObjectUpdateError where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignObjectUpdateError :: WriteForeign ObjectUpdateError where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- ObjectUpdateMessage
instance dataObjectRefObjectUpdateMessage :: DataObjectRef ObjectUpdateMessage where
  ref (ObjectUpdateMessage {ref: msgRef}) = msgRef
derive newtype instance readForeignObjectUpdateMessage :: ReadForeign ObjectUpdateMessage
derive newtype instance writeForeignObjectUpdateMessage :: WriteForeign ObjectUpdateMessage

------------------------------------------------------------------------------
-- ObjectUpdateResponse
derive instance eqObjectUpdateResponse :: Eq ObjectUpdateResponse
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
