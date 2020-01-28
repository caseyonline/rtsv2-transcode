module Rtsv2App.Data.PaginatedArray where

type PaginatedArray a =
  { total :: Int 
  , body :: Array a
  }
