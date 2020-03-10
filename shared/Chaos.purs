module Shared.Chaos
       (
         ChaosPayload
       , ChaosName(..)
       , ChaosGprocName(..)
       , defaultKill

       ) where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Foreign (Foreign, F)
import Shared.Stream as Stream
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data ChaosGprocName = String String
                    | Atom String
                    | SlotId Stream.SlotId
                    | SlotRole Stream.SlotRole
                    | GprocTuple2 ChaosGprocName ChaosGprocName
                    | GprocTuple3 ChaosGprocName ChaosGprocName ChaosGprocName
                    | GprocTuple4 ChaosGprocName ChaosGprocName ChaosGprocName ChaosGprocName

data ChaosName = Local String
               | Gproc ChaosGprocName

type ChaosPayload =
  { name :: ChaosName
  , exit_reason :: Maybe String
  , num_hits :: Maybe Int
  , delay_between_hits_ms :: Maybe Int
  }

defaultKill :: ChaosName -> ChaosPayload
defaultKill name = { name
                   , exit_reason: Nothing
                   , num_hits: Nothing
                   , delay_between_hits_ms: Nothing
                   }


------------------------------------------------------------------------------
-- ChaosName
instance readForeignChaosName :: ReadForeign ChaosName where
   readImpl f =
     let
       readLocal = readImpl :: Foreign -> F {local :: String}
       readGproc = readImpl :: Foreign -> F {gproc :: ChaosGprocName}
     in
      (Local <$> _.local <$> readLocal f)
      <|> (Gproc <$> _.gproc <$> readGproc f)

instance writeForeignChaosName :: WriteForeign ChaosName where
  writeImpl (Local str) = writeImpl {local: str}
  writeImpl (Gproc name) = writeImpl {gproc: name}

instance readForeignChaosGprocName :: ReadForeign ChaosGprocName where
  readImpl f =
    let
      readStr = readImpl :: Foreign -> F {string :: String}
      readAtom = readImpl :: Foreign -> F {atom :: String}
      readSlotId = readImpl :: Foreign -> F {slotId :: Stream.SlotId}
      readSlotRole = readImpl :: Foreign -> F {slotRole :: Stream.SlotRole}
      readTuple2 = readImpl :: Foreign -> F {gprocTuple2a :: ChaosGprocName, gprocTuple2b :: ChaosGprocName}
      readTuple3 = readImpl :: Foreign -> F {gprocTuple3a :: ChaosGprocName, gprocTuple3b :: ChaosGprocName, gprocTuple3c :: ChaosGprocName}
      readTuple4 = readImpl :: Foreign -> F {gprocTuple4a :: ChaosGprocName, gprocTuple4b :: ChaosGprocName, gprocTuple4c :: ChaosGprocName, gprocTuple4d :: ChaosGprocName}
    in
     (String <$> _.string <$> readStr f)
     <|> (Atom <$> _.atom <$> readAtom f)
     <|> (SlotId <$> _.slotId <$> readSlotId f)
     <|> (SlotRole <$> _.slotRole <$> readSlotRole f)
     <|> ((\{gprocTuple2a, gprocTuple2b} -> GprocTuple2 gprocTuple2a gprocTuple2b) <$> readTuple2 f)
     <|> ((\{gprocTuple3a, gprocTuple3b, gprocTuple3c} -> GprocTuple3 gprocTuple3a gprocTuple3b gprocTuple3c) <$> readTuple3 f)
     <|> ((\{gprocTuple4a, gprocTuple4b, gprocTuple4c, gprocTuple4d} -> GprocTuple4 gprocTuple4a gprocTuple4b gprocTuple4c gprocTuple4d) <$> readTuple4 f)

instance writeForeignChaosGprocName :: WriteForeign ChaosGprocName where
  writeImpl (String string) = writeImpl {string}
  writeImpl (Atom atom) = writeImpl {atom}
  writeImpl (SlotId slotId) = writeImpl {slotId}
  writeImpl (SlotRole slotRole) = writeImpl {slotRole}
  writeImpl (GprocTuple2 fst snd) = writeImpl { gprocTuple2a: fst
                                              , gprocTuple2b: snd
                                              }
  writeImpl (GprocTuple3 fst snd thd) = writeImpl { gprocTuple3a: fst
                                                  , gprocTuple3b: snd
                                                  , gprocTuple3c: thd
                                                  }
  writeImpl (GprocTuple4 fst snd thd fth) = writeImpl { gprocTuple4a: fst
                                                      , gprocTuple4b: snd
                                                      , gprocTuple4c: thd
                                                      , gprocTuple4d: fth
                                                      }
