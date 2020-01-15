 module Rtsv2.Agents.IngestInstanceSup
  ( isAvailable
  , startLink
  , startIngest
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Names as Names
import Shared.Stream (StreamAndVariant)
import Shared.LlnwApiTypes (StreamDetails)

isAvailable :: Effect Boolean
isAvailable = Names.isRegistered serverName

serverName :: SupervisorName
serverName = Names.ingestInstanceSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startIngest :: StreamDetails -> StreamAndVariant -> Effect Unit
startIngest streamDetails streamAndVariant = do
  result <- Sup.startSimpleChild childTemplate serverName (Tuple streamDetails streamAndVariant)
  case result of
    Pinto.AlreadyStarted pid -> pure unit
    Pinto.Started pid -> pure unit

init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "ingestAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
            : nil
        )

childTemplate :: Pinto.ChildTemplate (Tuple StreamDetails StreamAndVariant)
childTemplate = Pinto.ChildTemplate (IngestInstance.startLink)
