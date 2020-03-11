module Rtsv2.Handler.IntraPoP
  ( leader
  , publicState
  , slotState
  , testHelper
  ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.List (List, catMaybes, concat, nil, nub, null, (:))
import Logger (spy)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Shared.JsonLd as JsonLd
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr)
import Shared.Stream (AggregatorKey(..), ProfileName, SlotId, SlotRole(..))
import Shared.Types (Server, ServerAddress, extractAddress)
import Shared.Types.Agent.State as PublicState
import Simple.JSON (class ReadForeign)
import SpudGun as SpudGun
import StetsonHelper (GetHandler, PostHandler, jsonResponse, processPostPayload, textResponse)

leader :: GetHandler String
leader = textResponse do
  mLeader <- IntraPoPAgent.getCurrentTransPoPLeader
  pure $ (unwrap <<< extractAddress <$> mLeader)

testHelper :: PostHandler IntraPoP.TestHelperPayload
testHelper =  processPostPayload IntraPoP.testHelper

publicState :: GetHandler (PublicState.IntraPoP List)
publicState = jsonResponse $ Just <$> IntraPoP.getPublicState

type IngestAggregatorL = PublicState.IngestAggregator List
type IngestL = PublicState.Ingest List
type StreamRelayL = PublicState.StreamRelay List

slotState :: SlotId -> GetHandler { aggregators :: List IngestAggregatorL
                                  , ingests :: List IngestL
                                  , originRelays :: List StreamRelayL
                                  , downstreamRelays :: List StreamRelayL
                                  , egests :: List PublicState.Egest
                                  }
slotState slotId =
  jsonResponse getSlotState
  where
    getSlotState = do
      aggregatorServers <- getAggregatorServers slotId (Primary : Backup : nil)
      aggregators <- getAggregators slotId aggregatorServers
      ingests <- getIngests slotId aggregators
      originRelays <- getOriginRelays slotId aggregators
      downstreamRelays <- getDownstreamRelays slotId originRelays
      egests <- getEgests slotId (originRelays <> downstreamRelays)
      pure $ Just { aggregators
                  , ingests
                  , originRelays
                  , downstreamRelays
                  , egests: nub egests
                  }

getAggregatorServers :: SlotId -> List SlotRole -> Effect (List (Tuple SlotRole Server))
getAggregatorServers slotId roles =
  catMaybes <$> traverse (\role -> (map (Tuple role)) <$> IntraPoP.whereIsIngestAggregator (AggregatorKey slotId role)) roles

getAggregators :: SlotId -> List (Tuple SlotRole Server) -> Effect (List IngestAggregatorL)
getAggregators slotId servers =
  catMaybes <$> traverse (\(Tuple role server) -> getAggregator slotId role server) servers

getAggregator :: SlotId -> SlotRole -> Server -> Effect (Maybe (IngestAggregatorL))
getAggregator slotId slotRole server = getJson server (IngestAggregatorE slotId slotRole)

getIngests :: SlotId -> List (IngestAggregatorL) -> Effect (List IngestL)
getIngests slotId aggregators =
  catMaybes <$> concat <$> traverse (\{role, activeProfiles} ->
                                      traverse (\profileNode ->
                                                 let
                                                   {resource: {profileName, serverAddress}} = unwrap profileNode
                                                 in
                                                    getIngest slotId role profileName serverAddress
                                                ) activeProfiles
                                    ) (JsonLd.unwrapNode <$> aggregators)

getIngest :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> Effect (Maybe (IngestL))
getIngest slotId slotRole profileName server = getJson' server (IngestInstanceE slotId slotRole profileName)

getOriginRelays :: SlotId -> List (IngestAggregatorL) -> Effect (List StreamRelayL)
getOriginRelays slotId aggregators =
  catMaybes <$> concat <$> traverse (\{role, downstreamRelays} ->
                                      traverse (\deliverToNode ->
                                                 let
                                                   {resource: {server: relayServer}} = unwrap deliverToNode
                                                   {address: serverAddress} = unwrap relayServer
                                                 in
                                                    getRelay slotId role serverAddress
                                                ) downstreamRelays
                                    ) (JsonLd.unwrapNode <$> aggregators)

getDownstreamRelays :: SlotId -> List (StreamRelayL) -> Effect (List StreamRelayL)
getDownstreamRelays slotId upstreamRelays = do
  downStreamRelays <- catMaybes <$> concat <$> traverse (\{relaysServed} ->
                                                          traverse (\deliverToNode ->
                                                                     let
                                                                       {resource: {address: serverAddress}} = unwrap deliverToNode
                                                                     in
                                                                        getRelay slotId Primary serverAddress  -- TODO - StreamRelayL needs role
                                                                    ) relaysServed
                                                        ) (JsonLd.unwrapNode <$> upstreamRelays)
  if null downStreamRelays then
    pure nil
  else do
    nextLevel <- getDownstreamRelays slotId downStreamRelays
    pure $ downStreamRelays <> nextLevel

getRelay :: SlotId -> SlotRole -> ServerAddress -> Effect (Maybe (StreamRelayL))
getRelay slotId slotRole address = getJson' address (RelayStatsE slotId slotRole)

getEgests :: SlotId -> List (StreamRelayL) -> Effect (List PublicState.Egest)
getEgests slotId relays =
  catMaybes <$> concat <$> traverse (\{egestsServed} ->
                                      traverse (\deliverToNode ->
                                                 let
                                                   {resource: {address: serverAddress}} = unwrap deliverToNode
                                                 in
                                                    getEgest slotId serverAddress
                                                ) egestsServed
                                    ) (JsonLd.unwrapNode <$> relays)

getEgest :: SlotId -> ServerAddress -> Effect (Maybe PublicState.Egest)
getEgest slotId address = getJson' address (EgestStatsE slotId)

getJson :: forall a. ReadForeign a => Server -> Endpoint -> Effect (Maybe a)
getJson server endpoint =
  hush <$> SpudGun.bodyToJSON <$> SpudGun.getJson (makeUrl server endpoint)

getJson' :: forall a. ReadForeign a => ServerAddress -> Endpoint -> Effect (Maybe a)
getJson' address endpoint =
  hush <$> SpudGun.bodyToJSON <$> SpudGun.getJson (makeUrlAddr address endpoint)
