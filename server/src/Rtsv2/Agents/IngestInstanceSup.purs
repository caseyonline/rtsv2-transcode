 module Rtsv2.Agents.IngestInstanceSup
  ( isAvailable
  , startLink
  , startIngest
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (Pid)
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Names as Names
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant)

isAvailable :: Effect Boolean
isAvailable = Pinto.isRegistered serverName

serverName :: SupervisorName
serverName = Names.ingestInstanceSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startIngest :: StreamDetails -> StreamAndVariant -> Pid -> Effect Unit
startIngest streamDetails streamAndVariant handlerPid = do
  result <- Sup.startSimpleChild childTemplate serverName {streamDetails, streamAndVariant, handlerPid}
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

childTemplate :: Pinto.ChildTemplate IngestInstance.Args
childTemplate = Pinto.ChildTemplate (IngestInstance.startLink)
