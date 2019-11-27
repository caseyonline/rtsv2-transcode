{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "generics-rep"
    , "http"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-readline"
    , "psci-support"
    , "quickcheck"
    , "simple-json"
    , "spec"
    , "test-unit"
    , "toppokki"
    , "tuples"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
