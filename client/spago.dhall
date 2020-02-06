{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "rsvt2-app"
, packages =
    ./packages.dhall
, dependencies =
    [ "aff"
    , "aff-bus"
    , "affjax"
    , "argonaut-codecs"
    , "argonaut-core"
    , "console"
    , "css"
    , "debug"
    , "effect"
    , "formatters"
    , "halogen"
    , "halogen-css"
    , "halogen-formless"
    , "halogen-select"
    , "milkis"
    , "nonempty"
    , "now"
    , "precise-datetime"
    , "prelude"
    , "random"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    , "simple-json-generics"
    , "typelevel-prelude"
    , "variant"
    ]
}
