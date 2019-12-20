let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.5-20191204/src/packages.dhall sha256:308bc2dd2c5700e6e22fae85b10472fd76ddaaf8034711ff0eaede145573479a

let overrides =
      { erl-pinto =
          { dependencies =
              [ "erl-cowboy", "erl-process" ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version =
              "9de39f72b0b0e60f2fc86901361d9185e44a1a9c"
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
      , erl-maps =
          { dependencies =
              [ "assert"
              , "console"
              , "erl-lists"
              , "functions"
              , "prelude"
              , "tuples"
              , "unfoldable"
              ]
          , repo =
              "ssh://git@github.com/srstrong/purescript-erl-maps.git"
          , version =
              "33b4d45ec2c1c951dae6358b187ba013c128870b"
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
          { dependencies =
              [ "erl-cowboy", "erl-lager" ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-stetson.git"
          , version =
              "8688462267530254365bda284f06a162672de0c1"
          }
      }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
