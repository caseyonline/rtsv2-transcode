let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.5-20191204/src/packages.dhall sha256:308bc2dd2c5700e6e22fae85b10472fd76ddaaf8034711ff0eaede145573479a

let overrides =
      { erl-pinto =
          { dependencies =
              [ "erl-cowboy", "erl-process" ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version =
              "7ab2b1e0b3cbee664c4d1dad27a2a218b0aa23ab"
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
          , repo =
              "ssh://git@github.com/srstrong/purescript-simple-json.git"
          , version =
              "35a225860f49c4451dc87f0e9b941f319044079f"
          }
      , simple-json-generics =
          { dependencies =
              [ "simple-json" ]
          , repo =
              "ssh://git@github.com/justinwoo/purescript-simple-json-generics.git"
          , version =
              "f7127b94bd2da73b28e863c299edb72a42ee4bce"
          }
      , erl-maps =
          { dependencies =
              [ "console"
              , "erl-lists"
              , "functions"
              , "prelude"
              , "tuples"
              , "unfoldable"
              ]
          , repo =
              "https://github.com/purerl/purescript-erl-maps"
          , version =
              "d8153fdd97acca910b86bd44f55b6fce2f775264"
          }
      , erl-tuples =
          { dependencies =
              [ "assert", "console", "functions", "prelude", "tuples" ]
          , repo =
              "ssh://git@github.com/srstrong/purescript-erl-tuples.git"
          , version =
              "d923dab0d94af6872483bb5952b2652afdc3e95a"
          }
      , erl-stetson =
            upstream.erl-stetson
          ⫽ { version =
                "914803225f19533c7962e0aab5d498458370bad7"
            , dependencies =
                [ "erl-cowboy", "routing-duplex" ]
            }
      , erl-cowboy =
          { dependencies =
              [ "erl-process", "erl-modules" ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-cowboy.git"
          , version =
              "81e7e8ed5c89aaaacb9df23f181a325811969002"
          }
      , strings =
          upstream.strings ⫽ { version = "v4.0.1-erl1" }
      , record-prefix =
          { dependencies =
              [ "prelude", "heterogeneous", "console", "typelevel-prelude" ]
          , repo =
              "ssh://git@github.com/dariooddenino/purescript-record-prefix.git"
          , version =
              "v1.0.0"
          }
      , profunctor-lenses =
          { dependencies =
              [ "prelude" ]
          , repo =
              "ssh://git@github.com/purerl/purescript-profunctor-lenses.git"
          , version =
              "1edb1d375c49486c41b46371deed09e6512c2724"
          }
      }

let additions =
      { routing-duplex =
          { dependencies =
              [ "arrays"
              , "control"
              , "either"
              , "generics-rep"
              , "globals"
              , "lazy"
              , "prelude"
              , "profunctor"
              , "record"
              , "strings"
              , "typelevel-prelude"
              ]
          , repo =
              "ssh://git@github.com/natefaubion/purescript-routing-duplex.git"
          , version =
              "eceaba4ee9921250cc6640d5ead98bbfc44310f5"
          }
      , heterogeneous =
          { dependencies =
              [ "prelude", "record", "tuples", "functors", "variant", "either" ]
          , repo =
              "ssh://git@github.com/natefaubion/purescript-heterogeneous.git"
          , version =
              "854e6fe8e52c570954c22275aaf657ef068c73f8"
          }
      }

in  upstream ⫽ overrides ⫽ additions
