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
    , "http"
    , "psci-support"
    , "spec"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
