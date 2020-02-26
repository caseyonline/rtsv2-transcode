let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.5-20191204/src/packages.dhall sha256:308bc2dd2c5700e6e22fae85b10472fd76ddaaf8034711ff0eaede145573479a

let overrides =
      { erl-pinto =
          { dependencies =
              [ "erl-cowboy", "erl-process" ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version =
              "563688db6020bea1b7595fddf7b70dcb391741c6"
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
              "ssh://git@github.com/adrianroe/purescript-simple-json.git"
          , version =
              "c1c7e23a60b22c4813d56abb6600ce7a5eb66c55"
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
              "ssh://git@github.com/adrianroe/purescript-erl-maps.git"
          , version =
              "88995fa67be79f7dd77734f20b13bb54fec10f88"
          }
      , erl-tuples =
          { dependencies =
              [ "assert", "console", "functions", "prelude", "tuples" ]
          , repo =
              "ssh://git@github.com/srstrong/purescript-erl-tuples.git"
          , version =
              "d923dab0d94af6872483bb5952b2652afdc3e95a"
          }
      , strings =
          { dependencies =
              [ "prelude" ]
          , repo =
              "ssh://git@github.com/purerl/purescript-strings.git"
          , version =
              "1ef7b4969f202dedcc3e1089d950a9ce270d4fdc"
          }
      , erl-stetson =
          { dependencies =
              [ "erl-cowboy" ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-stetson.git"
          , version =
              "dfe1aa594822708e2593000523d29881d25d4ef5"
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
