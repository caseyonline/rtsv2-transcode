module Rtsv2.Node
       (
         Config
       )

       where

import Erl.Data.List (List)
import Shared.Agents (Agent)

type Config = { agents :: List Agent
              }
