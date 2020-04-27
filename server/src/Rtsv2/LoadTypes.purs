module Rtsv2.LoadTypes
 ( LoadAgentCosts(..)
 , LoadCheckResult(..)
 , LoadVariableCost(..)
 , LoadVariableCosts(..)
 , LoadFixedCosts(..)
 , LoadLimits(..)
 , LoadCosts(..)
 , LoadWatermarks(..)
 ) where

import Prelude

import Control.Apply (lift2)
import Data.Newtype (class Newtype, unwrap, wrap)
import Erl.Data.Tuple (Tuple2, tuple2, uncurry2)
import Erl.Utils as Erl
import Foreign (F, Foreign)
import Shared.Types (NetworkBPS, Percentage, SpecInt)
import Simple.JSON (class ReadForeign, read')


data LoadCheckResult = Green
                     | Amber
                     | Red

newtype LoadWatermarks = LoadWatermarks { lowWaterMark :: Percentage
                                        , highWaterMark :: Percentage
                                        }

newtype LoadFixedCosts = LoadFixedCosts { cpu :: SpecInt
                                        , network :: NetworkBPS
                                        }

newtype LoadVariableCost = LoadVariableCost (Tuple2 Number Number)

newtype LoadVariableCosts = LoadVariableCosts { cpu :: LoadVariableCost
                                              , network :: LoadVariableCost
                                              }

newtype LoadAgentCosts = LoadAgentCosts { fixed :: LoadFixedCosts
                                        , variable :: LoadVariableCosts
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
derive instance eqLoadCheckResult :: Eq LoadCheckResult
instance ordLoadCheckResult :: Ord LoadCheckResult where
  compare Green Green = EQ
  compare Green _ = LT
  compare _ Green = GT
  compare Amber Amber = EQ
  compare Amber _ = LT
  compare _ Amber = GT
  compare Red Red = EQ

derive instance newtypeLoadWatermarks :: Newtype LoadWatermarks _
derive newtype instance readForeignLoadWatermarks :: ReadForeign LoadWatermarks

derive instance newtypeLoadLimits :: Newtype LoadLimits _
derive newtype instance readForeignLoadLimits :: ReadForeign LoadLimits

derive instance newtypeLoadCosts :: Newtype LoadCosts _
derive newtype instance readForeignLoadCosts :: ReadForeign LoadCosts

derive instance newtypeLoadAgentCosts :: Newtype LoadAgentCosts _
derive newtype instance readForeignLoadAgentCosts :: ReadForeign LoadAgentCosts

derive instance newtypeLoadFixedCosts :: Newtype LoadFixedCosts _
derive newtype instance readForeignLoadFixedCosts :: ReadForeign LoadFixedCosts

derive instance newtypeLoadVariableCosts :: Newtype LoadVariableCosts _
derive newtype instance readForeignLoadVariableCosts :: ReadForeign LoadVariableCosts

instance readForeignLoadVariableCost :: ReadForeign LoadVariableCost where
  readImpl o =
    (uncurry2 toLoadVariable) =<< (Erl.readTuple2 o)
    where
      toLoadVariable :: Foreign -> Foreign -> F LoadVariableCost
      toLoadVariable f1 f2 =
        LoadVariableCost <$> lift2 tuple2 (readNumber f1) (readNumber f2)
      readNumber :: Foreign -> F Number
      readNumber f = read' f


instance semigroupLoadFixedCosts :: Semigroup LoadFixedCosts where
  append (LoadFixedCosts {cpu: cpuX, network: networkX}) (LoadFixedCosts {cpu: cpuY, network: networkY}) =
    LoadFixedCosts {cpu: wrap (unwrap cpuX + unwrap cpuY), network: wrap (unwrap networkX + unwrap networkY)}
