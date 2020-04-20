module Helpers.Log where

import Prelude

import Control.Monad.State (StateT, evalStateT, lift)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (spy)
import Effect.Aff (Aff, delay, throwError)
import Effect.Exception (error) as Exception
import Effect.Unsafe (unsafePerformEffect)
import Helpers.Env (currentTime)

throwSlowError :: forall e. String -> Aff e
throwSlowError e = do
  _ <- delay (Milliseconds 200.0)
  throwError $ Exception.error e

maybeLogStep :: forall a. String -> a -> Unit
maybeLogStep s a = do
  let _ = spy ((unsafePerformEffect currentTime) <> ": " <> s) a
  unit


as :: forall r. String -> Either String r -> Aff Unit
as desc val =
  case val of
    Right r -> do
      let _ = maybeLogStep "step" desc
      pure $ unit
    Left err ->
      throwSlowError $ (unsafePerformEffect currentTime)
                     <> ": Step: \""
                     <> desc
                     <> "\" failed with reason: "
                     <> err


as' :: forall a. String -> a -> Aff Unit
as' desc _ = do
  let _ = maybeLogStep "step" desc
  pure $ unit


asT :: forall r s. String -> Either String r -> StateT s Aff Unit
asT desc val =
  case val of
    Right r -> do
      let _ = maybeLogStep "step" desc
      pure $ unit
    Left err ->
      lift $ throwSlowError
           $ (unsafePerformEffect currentTime)
             <> ": Step: \""
             <> desc
             <> "\" failed with reason: "
             <> err

asT' :: forall a b. String -> b -> StateT a Aff Unit
asT' desc _ = do
  let _ = maybeLogStep "step" desc
  pure $ unit
