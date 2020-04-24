module Helpers.CreateString where

import Prelude

import Data.Array (catMaybes, filter, intercalate)
import Data.Map as Map
import Data.Maybe
import Data.Newtype (unwrap)
import Helpers.Types (Node(..), NodeAddress(..), PoPInfo)
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr)
import Shared.Types (ServerAddress(..))

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
                        <> intercalate ", " (quote <<< toAddrFromNode <$> nodesThisPoP)
                        <> "]}"

    mkRegion :: String -> (Array String) -> Maybe String
    mkRegion name [] = Nothing
    mkRegion name pops = Just $
      quote name <> ": {"
                 <> intercalate ", " pops
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

makeUrlAndUnwrap :: Node -> Endpoint -> String
makeUrlAndUnwrap node path = unwrap $ makeUrl (mkServerAddress node) path

toIfaceIndexString :: Node -> String
toIfaceIndexString (Node popNum nodeNum) = show (popNum * 10) <> show nodeNum
