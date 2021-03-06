{-
Welcome to a Spago project!
You can edit this file as you like.
-}

{ name = "rtsv2"
, backend = "purerl"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "erl-atom"
    , "erl-binary"
    , "erl-cowboy"
    , "erl-file"
    , "erl-lists"
    , "erl-maps"
    , "erl-pinto"
    , "erl-process"
    , "erl-stetson"
    , "erl-tuples"
    , "generics-rep"
    , "heterogeneous"
    , "kishimen"
    , "longs"
    , "partial"
    , "profunctor-lenses"
    , "psci-support"
    , "random"
    , "record-prefix"
    , "routing-duplex"
    , "simple-json"
    , "simple-json-generics"
    , "undefinable"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
