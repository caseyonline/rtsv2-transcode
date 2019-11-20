{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "erl-atom"
    , "erl-binary"
    , "erl-cowboy"
    , "erl-lager"
    , "erl-lists"
    , "erl-maps"
    , "erl-pinto"
    , "erl-process"
    , "erl-stetson"
    , "erl-tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
