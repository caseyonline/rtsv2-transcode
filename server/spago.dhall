{-
Welcome to a Spago project!
You can edit this file as you like.
-}

{ name =
    "rtsv2"
, backend =
    "purerl"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "erl-atom"
    , "erl-binary"
    , "erl-cowboy"
    , "erl-lists"
    , "erl-maps"
    , "erl-pinto"
    , "erl-process"
    , "erl-stetson"
    , "erl-tuples"
    , "generics-rep"
    , "heterogeneous"
    , "record-prefix"
    , "partial"
    , "psci-support"
    , "random"
    , "routing-duplex"
    , "simple-json"
    , "simple-json-generics"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
