module Rtsv2Sup where

import Effect
import Erl.Data.List
import Prelude

import Pinto as Pinto
import Pinto.Sup (startLink) as Sup
import Pinto.Sup 
import Rtsv2Web as Rtsv2Web
import Rtsv2Config as Rtsv2Config
import Rtsv2Library as Rtsv2Library

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "pure_sup" init

init :: Effect SupervisorSpec
init = do
  webPort <- Rtsv2Config.webPort
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren ( ( buildChild
                                       # childType Worker
                                       # childId "pure_web"
                                       # childStart Rtsv2Web.startLink  { webPort } )
                                       : 
                                       ( buildChild
                                       # childType Worker
                                       # childId "pure_library"
                                       # childStart Rtsv2Library.startLink {} )
                                        : nil)


