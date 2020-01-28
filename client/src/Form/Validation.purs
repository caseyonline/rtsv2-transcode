module Rtsv2App.Form.Validation where

import Prelude

import Rtsv2App.Data.Avatar (Avatar)
import Rtsv2App.Data.Avatar as Avatar
import Rtsv2App.Data.Email (Email(..))
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Data.Username as Username
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String
import Formless as F

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername
  | InvalidAvatar

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  InvalidAvatar -> "Invalid image URL"

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

-- | This validator ensures that an input string is longer than the provided lower limit. 
minLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

-- | This validator ensures that an input string is shorter than the provided upper limit. 
maxLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

-- | This validator ensures that an input string is a valid email address, using a fairly naive
-- | requirement that it at least includes the `@` symbol.
emailFormat :: ∀ form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ $ map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail

-- | This validator ensures that an input string is a valid username.
usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ note InvalidUsername <<< Username.parse

-- | Avatar validator follows the same pattern, validating and transforming an input string into
-- | an `Avatar`. 
avatarFormat :: ∀ form m. Monad m => F.Validation form m FormError String Avatar
avatarFormat = F.hoistFnE_ $ note InvalidAvatar <<< Avatar.parse 

-- Utilities

-- | Validation often relies on a true/false function (a predicate), where `true` should return the
-- | input value and `false` should return the correct error. This pattern happens often enough that
-- | I've created a small helper, `cond`, which abstracts the pattern.
cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err 

-- | This helper function lets us transform a set of validation rules so that they only apply when 
-- | the input is not empty. It isn't used in this module, but is used in the various forms. 
toOptional :: ∀ form m a b
   . Monoid a 
  => Eq a
  => Monad m 
  => F.Validation form m FormError a b
  -> F.Validation form m FormError a (Maybe b)
toOptional v = F.Validation \form val -> 
  case val == mempty of
    true -> pure (pure Nothing)
    _ -> (map <<< map) Just (F.runValidation v form val)
