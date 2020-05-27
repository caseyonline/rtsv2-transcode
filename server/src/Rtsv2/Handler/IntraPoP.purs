module Rtsv2.Handler.IntraPoP
  ( leader
  , publicState
  , slotState
  , testHelper
  ) where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.List (List, catMaybes, concat, nil, nub, null, (:))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Rtsv2.Config as Config
import Shared.JsonLd as JsonLd
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Stream (AggregatorKey(..), ProfileName, SlotId, SlotRole(..))
import Shared.Rtsv2.Types (Server, ServerAddress, extractAddress)
import Simple.JSON (class ReadForeign)
import SpudGun as SpudGun
import StetsonHelper (GetHandler, PostHandler, jsonResponse, processPostPayload, textResponse)



leader :: GetHandler String
leader = textResponse do
  mLeader <- IntraPoPAgent.getCurrentTransPoPLeader
  pure $ (unwrap <<< extractAddress <$> mLeader)

testHelper :: PostHandler IntraPoP.TestHelperPayload
testHelper = processPostPayload $ IntraPoP.testHelper >>> (map (const (Right unit :: Either Unit Unit)))

publicState :: GetHandler (PublicState.IntraPoP List)
publicState = jsonResponse $ Just <$> ( do
                                          webConfig <- Config.webConfig
                                          IntraPoP.getPublicState
                                      )

type IngestAggregatorL = PublicState.IngestAggregator List
type IngestL = PublicState.Ingest List
type StreamRelayL = PublicState.StreamRelay List

slotState :: SlotId -> GetHandler (PublicState.SlotState List)
slotState slotId =
  jsonResponse getSlotState
  where
    getSlotState = do
      aggregatorServers <- getAggregatorServers slotId (Primary : Backup : nil)
      aggregators <- getAggregators slotId aggregatorServers
      ingests <- getIngests slotId aggregators
      originRelays <- getOriginRelays slotId aggregators
      downstreamRelays <- getDownstreamRelays slotId originRelays
      egests <- getEgests slotId (Primary : Backup : nil) (originRelays <> downstreamRelays)
      pure $ Just { aggregators
                  , ingests
                  , originRelays
                  , downstreamRelays: nub downstreamRelays
                  , egests: nub egests
                  }

getAggregatorServers :: SlotId -> List SlotRole -> Effect (List (Tuple SlotRole Server))
getAggregatorServers slotId roles =
  catMaybes <$> traverse (\role -> (map (Tuple role)) <$> IntraPoP.whereIsIngestAggregator (AggregatorKey slotId role)) roles

getAggregators :: SlotId -> List (Tuple SlotRole Server) -> Effect (List IngestAggregatorL)
getAggregators slotId servers =
  catMaybes <$> traverse (\(Tuple role server) -> getAggregator slotId role server) servers

getAggregator :: SlotId -> SlotRole -> Server -> Effect (Maybe (IngestAggregatorL))
getAggregator slotId slotRole server = getJson server (Support.IngestAggregatorE slotId slotRole)

getIngests :: SlotId -> List (IngestAggregatorL) -> Effect (List IngestL)
getIngests slotId aggregators =
  catMaybes <$> concat <$> traverse (\ {role, activeProfiles} ->
                                      traverse (\profileNode ->
                                                 let
                                                   {resource: {profileName, serverAddress}} = unwrap profileNode
                                                 in
                                                    getIngest slotId role profileName serverAddress
                                                ) activeProfiles
                                    ) (JsonLd.unwrapNode <$> aggregators)

getIngest :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> Effect (Maybe (IngestL))
getIngest slotId slotRole profileName server = getJson' server (Support.IngestInstanceE slotId slotRole profileName)

getOriginRelays :: SlotId -> List (IngestAggregatorL) -> Effect (List StreamRelayL)
getOriginRelays slotId aggregators =
  catMaybes <$> concat <$> traverse (\ {role, downstreamRelays} ->
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
  downStreamRelays <- catMaybes <$> concat <$> traverse (\ {relaysServed} ->
                                                          traverse (\deliverToNode ->
                                                                     let
                                                                       {resource: {server: relayServer}} = unwrap deliverToNode
                                                                       {address: serverAddress} = unwrap relayServer
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
getRelay slotId slotRole address = getJson' address (Support.RelayStatsE slotId slotRole)

getEgests :: SlotId -> List SlotRole -> List (StreamRelayL) -> Effect (List (PublicState.Egest List))
getEgests slotId roles relays =
  catMaybes <$> concat <$> traverse (\ {egestsServed} ->
                                      concat <$> traverse (\deliverToNode ->
                                                            let
                                                              {resource: {address: serverAddress}} = unwrap deliverToNode
                                                            in
                                                                traverse (\slotRole -> getEgest slotId slotRole serverAddress) roles
                                                            ) egestsServed
                                    ) (JsonLd.unwrapNode <$> relays)

getEgest :: SlotId -> SlotRole -> ServerAddress -> Effect (Maybe (PublicState.Egest List))
getEgest slotId slotRole address = getJson' address (Support.EgestStatsE slotId slotRole)

getJson :: forall a. ReadForeign a => Server -> Support.Endpoint -> Effect (Maybe a)
getJson server endpoint = do
  url <- Support.makeUrl server endpoint
  hush <$> SpudGun.bodyToJSON <$> SpudGun.getJson url

getJson' :: forall a. ReadForeign a => ServerAddress -> Support.Endpoint -> Effect (Maybe a)
getJson' address endpoint = do
  url <- Support.makeUrlAddr address endpoint
  hush <$> SpudGun.bodyToJSON <$> SpudGun.getJson url
