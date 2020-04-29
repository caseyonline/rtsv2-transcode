module Rtsv2.LoadTypes
 ( LoadAgentCosts(..)
 , LoadCheckResult(..)
 , LoadVariableCost(..)
 , LoadFixedCost(..)
 , LoadLimits(..)
 , LoadCosts(..)
 , LoadWatermarks(..)
 , HardwareFactor(..)
 , ServerSelectionPredicate
 ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Erl.Data.List (List)
import Shared.Rtsv2.Types (NetworkKbps, Percentage, ServerLoad(..), SpecInt)
import Simple.JSON (class ReadForeign)

type ServerSelectionPredicate = ServerLoad -> LoadCheckResult

data LoadCheckResult = Green
                     | Amber
                     | Red

newtype LoadWatermarks = LoadWatermarks { lowWaterMark :: Percentage
                                        , highWaterMark :: Percentage
                                        }

newtype LoadFixedCost = LoadFixedCost { cpu :: SpecInt
                                      , network :: NetworkKbps
                                      }

newtype LoadVariableCost = LoadVariableCost { cpu :: SpecInt
                                            , network :: NetworkKbps
                                            }

newtype HardwareFactor = HardwareFactor { name :: String
                                        , cpuFactor :: Maybe Number
                                        , networkFactor :: Maybe Number
                                        }

newtype LoadAgentCosts = LoadAgentCosts { fixed :: LoadFixedCost
                                        , perProfile :: LoadVariableCost
                                        , perKbps :: LoadVariableCost
                                        , hardwareFactors :: List HardwareFactor
                                        }

newtype LoadLimits = LoadLimits { network :: LoadWatermarks
                                , cpu :: LoadWatermarks
                                }

newtype LoadCosts = LoadCosts { rtmpIngest :: LoadAgentCosts
                              , webRTCIngest :: LoadAgentCosts
                              , ingestAggregator :: LoadAgentCosts
                              , streamRelay :: LoadAgentCosts
                              , egest :: LoadAgentCosts
                              }

------------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- LoadCheckResult
derive instance eqLoadCheckResult :: Eq LoadCheckResult
instance ordLoadCheckResult :: Ord LoadCheckResult where
  compare Green Green = EQ
  compare Green _ = LT
  compare _ Green = GT
  compare Amber Amber = EQ
  compare Amber _ = LT
  compare _ Amber = GT
  compare Red Red = EQ

------------------------------------------------------------------------------
-- LoadWatermarks
derive instance newtypeLoadWatermarks :: Newtype LoadWatermarks _
derive newtype instance readForeignLoadWatermarks :: ReadForeign LoadWatermarks

------------------------------------------------------------------------------
-- LoadLimits
derive instance newtypeLoadLimits :: Newtype LoadLimits _
derive newtype instance readForeignLoadLimits :: ReadForeign LoadLimits

------------------------------------------------------------------------------
-- LoadCosts
derive instance newtypeLoadCosts :: Newtype LoadCosts _
derive newtype instance readForeignLoadCosts :: ReadForeign LoadCosts

------------------------------------------------------------------------------
-- HardwareFactor
derive instance newtypeHardwareFactor :: Newtype HardwareFactor _
derive newtype instance readForeignHardwareFactor :: ReadForeign HardwareFactor

------------------------------------------------------------------------------
-- LoadAgentsCosts
derive instance newtypeLoadAgentCosts :: Newtype LoadAgentCosts _
derive newtype instance readForeignLoadAgentCosts :: ReadForeign LoadAgentCosts

------------------------------------------------------------------------------
-- LoadFixedCost
derive instance newtypeLoadFixedCost :: Newtype LoadFixedCost _
derive newtype instance readForeignLoadFixedCost :: ReadForeign LoadFixedCost

instance semigroupLoadFixedCost :: Semigroup LoadFixedCost where
  append (LoadFixedCost {cpu: cpuX, network: networkX}) (LoadFixedCost {cpu: cpuY, network: networkY}) =
    LoadFixedCost {cpu: wrap (unwrap cpuX + unwrap cpuY), network: wrap (unwrap networkX + unwrap networkY)}

------------------------------------------------------------------------------
-- LoadVariableCost
derive instance newtypeLoadVariableCost :: Newtype LoadVariableCost _
derive newtype instance readForeignLoadVariableCost :: ReadForeign LoadVariableCost
