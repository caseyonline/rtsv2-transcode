-- -*- psc-ide-codegen: ("erl") -*-
module SpudGun 
( post
, post'
, put'
, getText
, getJson
, get'
) where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)

type Headers = List (Tuple2 String String)

foreign import post_ :: (String -> (Either String String)) -> (String -> (Either String String)) -> String -> String -> Headers -> Effect (Either String String)
foreign import put_ :: (String -> (Either String String)) -> (String -> (Either String String)) -> String -> String -> Headers -> Effect (Either String String)

post' :: String -> String -> Headers -> Effect (Either String String)
post' = post_ Left Right

put' :: String -> String -> Headers -> Effect (Either String String)
put' = put_ Left Right

post :: String -> String -> Effect (Either String String)
post s b = post' s b (tuple2 "Content-Type" "application/json" : tuple2 "Accept" "application/json" : nil)

foreign import get_ :: Maybe String -> (String -> Maybe String) -> String -> Headers -> Effect (Maybe String)

get' :: String -> Headers -> Effect (Maybe String)
get' = get_ Nothing Just

getText :: String -> Effect (Maybe String)
getText s = get' s (tuple2 "Accept" "text/plain" : nil)

getJson :: String -> Effect (Maybe String)
getJson s = get' s (tuple2 "Accept" "application/json" : nil)

