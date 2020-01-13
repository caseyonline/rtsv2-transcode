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
    , "effect"
    , "formatters"
    , "halogen"
    , "halogen-formless"
    , "nonempty"
    , "now"
    , "precise-datetime"
    , "prelude"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    , "slug"
    , "struct"
    , "tolerant-argonaut"
    , "typelevel-prelude"
    , "variant"
    ]
}
