{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff-promise"
    , "affjax"
    , "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "foreign-generic"
    , "generics-rep"
    , "heterogeneous"
    , "milkis"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-readline"
    , "psci-support"
    , "quickcheck"
    , "routing-duplex"
    , "record-prefix"
    , "simple-json"
    , "simple-json-generics"
    , "spec"
    , "test-unit"
    , "toppokki"
    , "tuples"
    , "uri"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
