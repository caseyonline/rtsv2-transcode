module Rtsv2App where

import Prelude
import Rtsv2Sup as Rtsv2Sup

import Pinto.App as App

start = App.simpleStart Rtsv2Sup.startLink
