module Helpers.OsCmd
  ( runProc
  ) where

import Prelude

import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as ChildProcess.Exit
import Node.ReadLine as RL

-- | Run a process for it's side-effect
runProc
  :: String
  -> Array String
  -> Aff Unit
runProc cmd args = do
  process <-
    liftEffect $
      ChildProcess.spawn cmd args $
        ChildProcess.defaultSpawnOptions

  liftEffect do
    iface <- RL.createInterface (ChildProcess.stderr process) mempty
    RL.setLineHandler iface \line ->
      Console.error line

  liftEffect do
    iface <- RL.createInterface (ChildProcess.stdout process) mempty
    RL.setLineHandler iface \line ->
      Console.log line

  v <- AVar.empty
  liftEffect $
    ChildProcess.onExit process case _ of
      ChildProcess.Exit.Normally 0 ->
        launchAff_ $ AVar.tryPut unit v
      ChildProcess.Exit.Normally n -> do
        launchAff_ $ AVar.kill (error $ "Subcommand exited with: " <> show n) v
      ChildProcess.Exit.BySignal signal ->
        launchAff_ $ AVar.kill (error $ "Received Signal: " <> show signal) v
  AVar.take v


-- execCommand
--   ∷ ∀ e
--   . String
--   → String
--   → Eff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE | e) Unit
-- execCommand name command =
--    catchLog (name <> " threw an exception") $
--     runST do
--       let cmd = unsafePartial fromJust (uncons (split (Pattern " ") command))
--       output ← newSTRef ""
--       log ("Running: \"" <> command <> "\"")
--       cp ← spawn cmd.head cmd.tail defaultSpawnOptions

--       let stout = stdout cp
--           sterr = stderr cp

--       onDataString stout UTF8 \s →
--         modifySTRef output (_ <> s) $> unit

--       onDataString sterr UTF8 \s →
--         modifySTRef output (_ <> s) $> unit

--       onExit cp \e → case e of
--         Normally 0 → logColored Green (name <> " successful!")
--         Normally code → do
--           log =<< readSTRef output
--           logColored Red (name <> " errored with code: " <> show code)
--         BySignal _       → pure unit
