module Rtsv2.Agents.EgestInstance.WebRTCTypes
       ( Port
       , SSRC
       , RtcpReceptionStats
       , WebRTCManagerSessionId
       , WebRTCMediaChannelType
       , WebRTCMediaChannelStats
       , WebRTCSessionManagerStats
       , WebRTCStreamServerStats
       ) where

import Erl.Data.Map (Map)

type Port = Int

type SSRC = Int

type RtcpReceptionStats = { lostFraction :: Number
                          , lostTotal :: Int
                          , sequenceEpoch :: Int
                          , sequenceLatest :: Int
                          , interarrivalJitter :: Int
                          }

type WebRTCManagerSessionId = String

data WebRTCMediaChannelType = Audio
                            | Video

type WebRTCMediaChannelStats = { framesDroppedNoReturn :: Int
                               , framesDroppedNoCrypto :: Int
                               , localPort :: Port
                               , remotePort :: Port
                               , remoteAddress :: String
                               , octetsReceived :: Int
                               , octetsSent :: Int
                               , packetsSent :: Int
                               , incomingSsrc :: SSRC
                               , receivedPlis :: SSRC
                               , receiverInfo :: Map SSRC RtcpReceptionStats
                               }

type WebRTCSessionManagerStats = { channels :: Map WebRTCMediaChannelType WebRTCMediaChannelStats
                                 }

type WebRTCStreamServerStats a = { serverId :: a
                                 , sessionCount :: Int
                                 , sessionInfo :: Map WebRTCManagerSessionId WebRTCSessionManagerStats
                                 }
