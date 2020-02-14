 module Rtsv2.Agents.IngestInstanceSup
  ( isAvailable
  , startLink
  , startIngest
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (Pid)
import Pinto (SupervisorName, okAlreadyStarted)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Names as Names
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (IngestKey(..), StreamAndVariant(..))

isAvailable :: Effect Boolean
isAvailable = Pinto.isRegistered serverName

serverName :: SupervisorName
serverName = Names.ingestInstanceSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startIngest :: StreamDetails -> StreamAndVariant -> String -> Int -> Pid -> Effect Unit
startIngest streamDetails streamAndVariant@(StreamAndVariant streamId variant) remoteAddress remotePort handlerPid = do
  let
    ingestKey = IngestKey streamId streamDetails.role variant
  void <$> okAlreadyStarted =<< Sup.startSimpleChild childTemplate serverName { streamDetails
                                                                              , ingestKey
                                                                              , remoteAddress
                                                                              , remotePort
                                                                              , handlerPid
                                                                              }

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
