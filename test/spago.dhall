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
    , "node-readline"
    , "psci-support"
    , "quickcheck"
    , "spec"
    , "test-unit"
    , "toppokki"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
