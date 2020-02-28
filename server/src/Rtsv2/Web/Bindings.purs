module Rtsv2.Web.Bindings
       ( slotId
       , profileName
       , streamRole
       , slotNameAndProfileName
       , popName
       , shortName

       , slotIdBindingLiteral
       , profileNameBindingLiteral
       , streamRoleBindingLiteral
       , slotNameAndProfileNameBindingLiteral
       , popNameBindingLiteral
       , shortNameBindingLiteral
       ) where

import Prelude

import Data.Array ((!!))
import Data.Int (fromString)
import Data.Maybe (fromMaybe')
import Data.Newtype (wrap)
import Data.String (Pattern(..), split)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req, binding)
import Rtsv2.Router.Endpoint (parseSlotRole)
import Shared.Stream (ProfileName, RtmpShortName, SlotId, SlotNameAndProfileName(..), SlotRole)
import Shared.Types (PoPName)
import Shared.Utils (lazyCrashIfMissing)


slotId :: Req -> SlotId
slotId = wrap <<< fromMaybe' (lazyCrashIfMissing $ slotIdBindingLiteral <> " binding missing") <<< ((=<<) fromString) <<< binding (atom slotIdBindingLiteral)

profileName :: Req -> ProfileName
profileName = wrap <<< fromMaybe' (lazyCrashIfMissing $ profileNameBindingLiteral <> " binding missing") <<< binding (atom profileNameBindingLiteral)

streamRole :: Req -> SlotRole
streamRole req =
  let
    mSlotRole = parseSlotRole =<< binding (atom streamRoleBindingLiteral) req
  in
   fromMaybe' (lazyCrashIfMissing $ streamRoleBindingLiteral <> " binding missing or incorrect") mSlotRole


popName :: Req -> PoPName
popName = wrap <<< fromMaybe' (lazyCrashIfMissing $ popNameBindingLiteral <> " binding missing") <<< binding (atom popNameBindingLiteral)

shortName :: Req -> RtmpShortName
shortName = wrap <<< fromMaybe' (lazyCrashIfMissing $ shortNameBindingLiteral <> " binding missing") <<< binding (atom shortNameBindingLiteral)

slotNameAndProfileName :: Req -> SlotNameAndProfileName
slotNameAndProfileName req =
  let
    profileNameStr = fromMaybe' (lazyCrashIfMissing $ slotNameAndProfileNameBindingLiteral <> " binding missing") $ binding (atom slotNameAndProfileNameBindingLiteral) req
    slotIdStr = fromMaybe' (lazyCrashIfMissing $ slotNameAndProfileNameBindingLiteral <> " badly formed") $ (split (Pattern "_") profileNameStr) !! 0
  in
   SlotNameAndProfileName slotIdStr (wrap profileNameStr)

slotIdBindingLiteral :: String
slotIdBindingLiteral = "slot_id"

profileNameBindingLiteral :: String
profileNameBindingLiteral = "profile_name"

streamRoleBindingLiteral :: String
streamRoleBindingLiteral = "stream_role"

slotNameAndProfileNameBindingLiteral :: String
slotNameAndProfileNameBindingLiteral = "slotname_and_profilename"

popNameBindingLiteral :: String
popNameBindingLiteral = "pop_name"

shortNameBindingLiteral :: String
shortNameBindingLiteral = "short_name"
