module Helpers.Assert where

import Prelude

import Control.Monad.State.Class (class MonadState, gets, modify)
import Data.Array (length, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (un, unwrap)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Helpers.CreateString (toAddrFromNode, mkServerAddress, makeUrlAndUnwrap)
import Helpers.HTTP as HTTP
import Helpers.Types (ResWithBody(..), Node(..), TestNode)
import Milkis as M
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Stream (SlotId, SlotNameAndProfileName(..), SlotRole(..))
import Shared.Rtsv2.Types (ServerAddress(..), extractAddress)
import Shared.Rtsv2.Agent.State as PublicState
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON


assertFun :: forall a. Show a => (a -> Boolean) -> Either String a -> Aff (Either String a)
assertFun pred either =
  pure $ assertFun' pred either

assertFun' :: forall a. Show a => (a -> Boolean) -> Either String a -> Either String a
assertFun' pred either =
  assertFun'' pred =<< either

assertFun'' :: forall a. Show a => (a -> Boolean) -> a -> Either String a
assertFun'' pred subject =
  if pred subject
  then Right subject
  else Left $ "Predicate failed for content " <> (show subject)

assertSame :: forall a. Show a => Eq a => List a -> Aff (Either String Unit)
assertSame Nil = pure $ Right unit
assertSame (Cons x Nil) = pure $ Right unit
assertSame (Cons x t@(Cons y xs)) =
  if x == y
    then assertSame t
    else pure $ Left $ "\"" <> (show x) <> "\" is not the same as \"" <> (show y) <> "\""


-- | Response

assertBodyFun
  :: forall a. Show a
  => ReadForeign a
  => (a -> Boolean)
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertBodyFun pred left@(Left _) = pure left
assertBodyFun pred (Right rwb) = pure $ (parse rwb) >>= (assertFun'' pred) <#> (const rwb)
  where
    parse :: ReadForeign a => ResWithBody -> Either String a
    parse {body} = lmap (jsonError body) $ SimpleJSON.readJSON body
    jsonError body error = "Could not parse json " <> body <> " due to " <> show error

assertBodyText :: String -> Either String M.Response -> Aff (Either String M.Response)
assertBodyText expected either =
  case either of
    Left e -> pure $ Left e
    Right response -> do
      text <- M.text response
      if text == expected
        then pure either
        else pure $ Left $ "Body " <> text <> " did not match expected " <> expected

assertBodiesSame :: List ResWithBody -> Aff (Either String Unit)
assertBodiesSame Nil = pure $ Right unit
assertBodiesSame (Cons x xs) = pure $ assertBodiesSame_ x.body xs
  where
    assertBodiesSame_ :: String -> List ResWithBody -> Either String Unit
    assertBodiesSame_ _ Nil = Right unit
    assertBodiesSame_ firstBody (Cons y ys) =
      if y.body == firstBody
        then assertBodiesSame_ firstBody ys
        else Left $ "\"" <> firstBody <> "\" is not the same as \"" <> y.body <> "\""

assertStatusCode :: Int -> Either String ResWithBody -> Aff (Either String ResWithBody)
assertStatusCode expectedCode either =
  pure $ either
         >>= \rwb ->
               if rwb.statusCode == expectedCode
                 then either
                 else Left $  "Unexpected statuscode: Expected " <> show expectedCode <> ", got " <> show rwb.statusCode

assertHeader :: Tuple String String -> Either String ResWithBody -> Aff (Either String ResWithBody)
assertHeader (Tuple header value) either =
  pure $ either
         >>= \rwb@{headers} ->
               let
                 mEqual = Object.lookup header headers
                          >>= (\hdrVal -> if hdrVal == value then Just true else Nothing)

               in
                if isNothing mEqual
                  then Left $ "Header " <> header <> ":" <> value <> " not present in response " <> show headers
                  else either

-- | Relay

assertRelayForEgest
  :: Array Node
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertRelayForEgest = assertBodyFun <<< predicate
  where
    predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
    predicate servers streamRelayState = do
      let addr = sort $ (ServerAddress <<< toAddrFromNode) <$> servers
          addr' = sort (_.address <<< JsonLd.unwrapNode <$> (JsonLd.unwrapNode streamRelayState).egestsServed)
      addr == addr'

assertRelayForRelay
  :: Array Node
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertRelayForRelay = assertBodyFun <<< predicate
  where
    predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
    predicate servers streamRelayState = do
      let addr  = sort $ (ServerAddress <<< toAddrFromNode) <$> servers
          addr' = sort ( _.address
                         <<< unwrap
                         <<< _.server
                         <<< JsonLd.unwrapNode <$> (JsonLd.unwrapNode streamRelayState).relaysServed)
      addr == addr'

assertRelayCount
  :: SlotId
  -> Int
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertRelayCount requiredSlotId count = assertBodyFun $ predicate
  where
    predicate :: (PublicState.IntraPoP Array) -> Boolean
    predicate popState =
      let nodeAddresses = toAddrFromNode
          serverAddressesForSlotId =
            foldl (\acc {slotId, servers} ->
                    if slotId == requiredSlotId
                    then acc <> (extractAddress <<< JsonLd.unwrapNode <$> servers)
                    else acc
                  ) []  (JsonLd.unwrapNode popState).relayLocations
      in
       length (sort serverAddressesForSlotId) == count

-- | Aggregator

assertAggregator
  :: Array SlotNameAndProfileName
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertAggregator = assertBodyFun <<< predicate
  where
    predicate :: Array SlotNameAndProfileName -> PublicState.IngestAggregator Array -> Boolean
    predicate vars ingestAggregatorState = do
      let addr  = sort ((\(SlotNameAndProfileName _ profileName) -> profileName) <$> vars)
          addr' = sort $ (_.profileName <<< JsonLd.unwrapNode) <$> (JsonLd.unwrapNode ingestAggregatorState).activeProfiles
      addr == addr'

assertAggregatorOn
  :: Array Node
  -> SlotId
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertAggregatorOn nodes requiredSlotId =
  assertBodyFun $ predicate
  where
    predicate :: PublicState.IntraPoP Array -> Boolean
    predicate popState =
      let
        nodeAddresses = toAddrFromNode
        serverAddressesForSlotId = foldl (\acc {slotId, servers} ->
                                                 if slotId == requiredSlotId
                                                 then acc <> (extractAddress <<< JsonLd.unwrapNode <$> servers)
                                                 else acc
                                               ) []  (JsonLd.unwrapNode popState).aggregatorLocations
      in
       (sort $ (ServerAddress <<< toAddrFromNode) <$> nodes) == sort serverAddressesForSlotId

assertEgestClients
  :: Int
  -> Either String ResWithBody
  -> Aff (Either String ResWithBody)
assertEgestClients = assertBodyFun <<< predicate
  where
    predicate :: Int -> PublicState.Egest -> Boolean
    predicate count egestStats = count == (JsonLd.unwrapNode egestStats).clientCount

compareSlotState preFilter predicate either@(Left _) = pure either
compareSlotState preFilter predicate either@(Right slotState) = do
  currentSlotState <- gets (Map.lookup "slotState")
  if
    predicate (Just (preFilter slotState)) (preFilter <$> currentSlotState) then pure either
  else
    let
      _ = spy "lhs" currentSlotState
      _ = spy "rhs" slotState
    in
      pure $ Left "does not match"

dumpIntraPoP = assertBodyFun $ predicate
  where
    predicate :: PublicState.IntraPoP Array -> Boolean
    predicate popState =
      let _ = spy "popState" popState
      in true

dumpSlotState = assertBodyFun $ predicate
  where
    predicate :: PublicState.SlotState Array -> Boolean
    predicate slotState =
      let _ = spy "slotState" slotState
      in true
