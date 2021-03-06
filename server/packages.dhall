let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.13.6-20200513/packages.dhall sha256:a06d91d931d8f2b984b53e4fd5fae0d8a30f7c0ccf99b8516b9aaa356bc46169

let overrides =
      { erl-cowboy =
          { dependencies = [ "erl-modules" ]
          , repo = "https://github.com/id3as/purescript-erl-cowboy.git"
          , version = "4ee391f0349c00d92f68e4331425174eb8bdff9e"
          }
      , erl-stetson =
          { dependencies = [ "erl-cowboy" ]
          , repo = "https://github.com/id3as/purescript-erl-stetson.git"
          , version = "0a2b2b5ad4a013a71c9a2e39ae50cd9ec6c011cd"
          }
      , erl-pinto =
          { dependencies = [ "erl-cowboy", "erl-process" ]
          , repo = "https://github.com/id3as/purescript-erl-pinto.git"
          , version = "e08a7285eefa73e22869e24f2e366eb4890fe414"
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
              "https://github.com/justinwoo/purescript-simple-json-generics.git"
          , version = "f7127b94bd2da73b28e863c299edb72a42ee4bce"
          }
      , kishimen =
          { dependencies =
            [ "prelude", "generics-rep", "variant", "typelevel-prelude" ]
          , repo = "https://github.com/justinwoo/purescript-kishimen.git"
          , version = "8a9b11f9bcdaf4ff63c2e572def3d0b2a4e2c870"
          }
      , longs =
          { dependencies =
            [ "prelude"
            , "nullable"
            , "strings"
            , "foreign"
            , "functions"
            , "console"
            , "effect"
            ]
          , repo = "https://github.com/id3as/purescript-longs.git"
          , version = "100d382591bbbf912821576294f0849b695446d9"
          }
      , undefinable =
          { dependencies =
            [ "prelude", "heterogeneous", "console", "typelevel-prelude" ]
          , repo = "https://github.com/purerl/purescript-undefinable.git"
          , version = "582c6a697863c1e7f0fed5f45d91b4f8d3b5263f"
          }
      }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
