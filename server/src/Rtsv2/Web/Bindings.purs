module Rtsv2.Web.Bindings
       ( streamId
       , variant
       , streamAndVariant
       , popName


       , streamIdBindingLiteral
       , variantBindingLiteral
       , streamAndVariantBindingLiteral
       , popNameBindingLiteral
       ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe (fromMaybe')
import Data.Newtype (wrap)
import Data.String (Pattern(..), split)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req, binding)
import Shared.Stream (StreamAndVariant(..), StreamId, StreamVariant)
import Shared.Types (PoPName)
import Shared.Utils (lazyCrashIfMissing)


streamId :: Req -> StreamId
streamId = wrap <<< fromMaybe' (lazyCrashIfMissing $ streamIdBindingLiteral <> " binding missing") <<< binding (atom streamIdBindingLiteral)

variant :: Req -> StreamVariant
variant =  wrap <<< fromMaybe' (lazyCrashIfMissing $ variantBindingLiteral <> " binding missing") <<< binding (atom variantBindingLiteral)

popName :: Req -> PoPName
popName =  wrap <<< fromMaybe' (lazyCrashIfMissing $ popNameBindingLiteral <> " binding missing") <<< binding (atom popNameBindingLiteral)


streamAndVariant :: Req -> StreamAndVariant
streamAndVariant req =
  let
    variantIdStr = fromMaybe' (lazyCrashIfMissing $ streamAndVariantBindingLiteral <> " binding missing") $ binding (atom streamAndVariantBindingLiteral) req
    streamIdStr = fromMaybe' (lazyCrashIfMissing $ streamAndVariantBindingLiteral <> " badly formed") $ (split (Pattern "_") variantIdStr) !! 0
  in
   StreamAndVariant (wrap streamIdStr) (wrap variantIdStr)


streamIdBindingLiteral :: String
streamIdBindingLiteral = "stream_id"

variantBindingLiteral :: String
variantBindingLiteral = "variant_id"

popNameBindingLiteral :: String
popNameBindingLiteral = "pop_name"

streamAndVariantBindingLiteral :: String
streamAndVariantBindingLiteral = "stream_and_variant"
