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
    , "milkis"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-readline"
    , "psci-support"
    , "quickcheck"
    , "simple-json"
    , "simple-json-generics"
    , "spec"
    , "test-unit"
    , "toppokki"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
