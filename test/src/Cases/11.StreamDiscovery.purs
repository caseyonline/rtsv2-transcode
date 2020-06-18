module Cases.StreamDiscovery (streamDiscoveryTests) where

import Prelude

import Data.Int (toNumber)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.Assert as A
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.Types (Node)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Types (CanaryState(..))
import Test.Spec (SpecT, after_, before_, describe, it)
import Toppokki as T

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
streamDiscoveryTests :: forall m. Monad m => SpecT Aff Unit m Unit
streamDiscoveryTests = do
  describe "11 Stream discovery tests" do
    before_ (E.lookupPuppeteerEnv *> F.startSession nodes *> F.launch nodes) do
      after_ (F.stopSession *> F.stopSlot) do
        streamDiscoveryUrls
        streamDiscoveryCache

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2]

options :: { args :: Array String
           , devtools :: Boolean
           , headless :: Boolean
           }
options =
  { headless: false
  , args: E.browserLaunchArgsIng
  , devtools: true
  }

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
streamDiscoveryUrls :: forall m. Monad m => SpecT Aff Unit m Unit
streamDiscoveryUrls =
  it "11.1 Stream Discovery returns websocket urls " $ do

    HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.slot1Name
      >>= A.assertStatusCode 404
      >>= L.as "No ingest started so returns 404"


    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
      >>= A.assertStatusCode 200
      >>= L.as  "create ingest"

    E.waitForIntraPoPDisseminate
      >>= L.as' "let ingest presence disseminate"

    HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.slot1Name
      >>= A.assertStatusCode 200
      >>= A.assertBodyTextFun (\a -> a /= "")
      >>= L.as "Ingest discovery returns non empty URL"

streamDiscoveryCache :: forall m. Monad m => SpecT Aff Unit m Unit
streamDiscoveryCache =
  it "11.2 Stream Discovery caches slot lookups " $ do

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
      >>= A.assertBodyFun ((==) 0 <<< healthNodeToCacheTotal)
      >>= L.as "cache empty"

    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= L.as  "create ingest"

    E.waitForIntraPoPDisseminate                                          >>= L.as' "let ingest presence disseminate"

    HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.slot1Name
      >>= A.assertStatusCode 200
      >>= A.assertBodyTextFun (\a -> a /= "")
      >>= L.as "Ingest discovery returns non empty URL"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
      >>= A.assertBodyFun ((==) 1 <<< healthNodeToCacheTotal)
      >>= A.assertBodyFun ((==) 0.0 <<< healthNodeToCacheUtilization)
      >>= L.as "cache has one access, but utilization is zero"

    HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.slot1Name
      >>= A.assertStatusCode 200
      >>= A.assertBodyTextFun (\a -> a /= "")
      >>= L.as "Ingest discovery returns non empty URL"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
      >>= A.assertBodyFun ((==) 2 <<< healthNodeToCacheTotal)
      >>= A.assertBodyFun ((==) 0.5 <<< healthNodeToCacheUtilization)
      >>= L.as "cache has two accesses, utilization is 50%"

    E.waitForIntraPoPDisseminate                                          >>= L.as' "let slot lookup presence disseminate"

    HTTP.getStreamDiscovery E.p1n2 E.shortName1 E.slot1Name
      >>= A.assertStatusCode 200
      >>= A.assertBodyTextFun (\a -> a /= "")
      >>= L.as "Ingest discovery returns non empty URL"

    HTTP.healthCheck E.p1n2 >>= A.assertStatusCode 200
      >>= A.assertBodyFun ((==) 1 <<< healthNodeToCacheTotal)
      >>= A.assertBodyFun ((==) 1.0 <<< healthNodeToCacheUtilization)
      >>= L.as "cache has one entry, utilization is 100%"

healthNodeToCacheUtilization :: JsonLd.HealthNode Array -> Number
healthNodeToCacheUtilization =
  (\{slotLookupCacheUtilization: {cacheHits, cacheMisses}} -> (toNumber cacheHits) / (toNumber (cacheHits + cacheMisses)))
  <<< JsonLd.unwrapNode

healthNodeToCacheTotal :: JsonLd.HealthNode Array -> Int
healthNodeToCacheTotal =
  (\{slotLookupCacheUtilization: {cacheHits, cacheMisses}} -> cacheHits + cacheMisses)
  <<< JsonLd.unwrapNode
