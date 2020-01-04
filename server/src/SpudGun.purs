module SpudGun
( post
, post'
, put'
, getText
, getJson
, get'
) where

import Data.Either (Either(..))
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)

type Headers = List (Tuple2 String String)

foreign import postImpl :: (String -> (Either String String)) -> (String -> (Either String String)) -> String -> String -> Headers -> Effect (Either String String)
foreign import putImpl :: (String -> (Either String String)) -> (String -> (Either String String)) -> String -> String -> Headers -> Effect (Either String String)

post' :: String -> String -> Headers -> Effect (Either String String)
post' = postImpl Left Right

put' :: String -> String -> Headers -> Effect (Either String String)
put' = putImpl Left Right

post :: String -> String -> Effect (Either String String)
post s b = post' s b (tuple2 "Content-Type" "application/json" : tuple2 "Accept" "application/json" : nil)

foreign import getImpl :: (String -> (Either String String)) -> (String -> (Either String String))  -> String -> Headers -> Effect (Either String String)

get' :: String -> Headers -> Effect (Either String String)
get' = getImpl Left Right

getText :: String -> Effect (Either String String)
getText s = get' s (tuple2 "Accept" "text/plain" : nil)

getJson :: String -> Effect (Either String String)
getJson s = get' s (tuple2 "Accept" "application/json" : nil)
