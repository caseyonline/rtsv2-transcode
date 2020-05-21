module Helpers.CreateString where

import Prelude

import Data.Array (catMaybes, filter, intercalate)
import Data.Map as Map
import Data.Maybe
import Data.Newtype (unwrap)
import Helpers.Types (Node(..), NodeAddress(..), PoPInfo)
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Router.Endpoint.Public as Public
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Types (ServerAddress(..))

-------------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------------
toAddrFromNode :: Node -> String
toAddrFromNode (Node popNum nodeNum) = "172.16." <> show (popNum + 168) <> "." <> show nodeNum

-------------------------------------------------------------------------------
-- PoP
-------------------------------------------------------------------------------
mkPoPJsonString :: Array Node -> String
mkPoPJsonString nodes =
  let

    iad = {name: "iad", number: 1, x: 0.0, y: 0.0} :: PoPInfo
    dal = {name: "dal", number: 2, x: 0.0, y: 0.0} :: PoPInfo
    lax = {name: "lax", number: 3, x: 0.0, y: 0.0} :: PoPInfo
    fra = {name: "fra", number: 4, x: 0.0, y: 0.0} :: PoPInfo

    popNodes pop = filter (\(Node popNum  _) -> popNum == pop.number)

    insertNodes k [] acc = acc
    insertNodes k v acc = Map.insert k v acc

    nodeMap = Map.empty
              # insertNodes iad.number (popNodes iad nodes)
              # insertNodes dal.number (popNodes dal nodes)
              # insertNodes lax.number (popNodes lax nodes)
              # insertNodes fra.number (popNodes fra nodes)

    quote = show

    mkPoP :: PoPInfo -> Maybe String
    mkPoP pop =
      case Map.lookup pop.number nodeMap of
        Nothing -> Nothing
        Just nodesThisPoP -> Just $
          show pop.name <> ": { \"geoLoc\" : [" <> (quote $ show pop.x) <> ","
                        <> (quote $ show pop.y) <> "], \"nodes\": ["
                        <> intercalate ", " (mkNode <$> nodesThisPoP)
                        <> "]}"

    mkRegion :: String -> (Array String) -> Maybe String
    mkRegion name [] = Nothing
    mkRegion name pops = Just $
      quote name <> ": {"
                 <> (intercalate ", " pops)
                 <> "}"

    mkNode :: Node -> String
    mkNode node =
      "{"
      <> "\"address\":" <> ((quote <<< toAddrFromNode) node)
      <> ", \"maxCpuCapacity\": 1000"
      <> ", \"maxNetworkCapacity\": 1000000000"
      <> ", \"capabilityTags\": []"
      <> ", \"agents\": [\"TransPoP\", \"Ingest\", \"IngestAggregator\", \"StreamRelay\", \"Egest\"]"
      <> "}"

    america = mkRegion "americas" $ catMaybes $ mkPoP <$> [iad, dal, lax]
    europe  = mkRegion "europe"   $ catMaybes $ mkPoP <$> [fra]

  in
   "{"  <> (intercalate ", " $ catMaybes [america, europe]) <> "}"



-------------------------------------------------------------------------------
-- Url
-------------------------------------------------------------------------------
mkServerAddress :: Node -> NodeAddress
mkServerAddress node = NodeAddress {address: ServerAddress $ toAddrFromNode node}

makeUrlAndUnwrapSupport :: Node -> Support.Endpoint -> String
makeUrlAndUnwrapSupport node path = unwrap $ Support.makeUrl (mkServerAddress node) path

makeUrlAndUnwrapSystem :: Node -> System.Endpoint -> String
makeUrlAndUnwrapSystem node path = unwrap $ System.makeUrl (mkServerAddress node) path

makeUrlAndUnwrapPublic :: Node -> Public.Endpoint -> String
makeUrlAndUnwrapPublic node path = unwrap $ Public.makeUrl (mkServerAddress node) path

toIfaceIndexString :: Node -> String
toIfaceIndexString (Node popNum nodeNum) = show (popNum * 10) <> show nodeNum
