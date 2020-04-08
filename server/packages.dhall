let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.13.6-20200402/packages.dhall sha256:5442e50aa76c20bd60b2770ab41c68bae80f6ec96f2df1cfaea310673de567d1

let overrides =
      { erl-pinto =
          { dependencies = [ "erl-cowboy", "erl-process" ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version = "7ab2b1e0b3cbee664c4d1dad27a2a218b0aa23ab"
          }
      , simple-json =
          { dependencies =
            [ "assert"
            , "effect"
            , "erl-lists"
            , "erl-maps"
            , "exceptions"
            , "foldable-traversable"
            , "foreign"
            , "functions"
            , "generics-rep"
            , "globals"
            , "lists"
            , "nullable"
            , "ordered-collections"
            , "partial"
            , "prelude"
            , "random"
            , "record"
            , "strings"
            , "transformers"
            , "typelevel-prelude"
            , "variant"
            ]
          , repo = "https://github.com/purerl/purescript-simple-json"
          , version = "54becc3cf31c58a4ff56c6073ed0a200cc11e10d"
          }
      , simple-json-generics =
          { dependencies = [ "simple-json" ]
          , repo =
              "ssh://git@github.com/justinwoo/purescript-simple-json-generics.git"
          , version = "f7127b94bd2da73b28e863c299edb72a42ee4bce"
          }
      , kishimen =
          { dependencies =
            [ "prelude", "generics-rep", "variant", "typelevel-prelude" ]
          , repo = "ssh://git@github.com/justinwoo/purescript-kishimen.git"
          , version = "8a9b11f9bcdaf4ff63c2e572def3d0b2a4e2c870"
          }
      }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
