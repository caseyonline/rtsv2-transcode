let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191110/packages.dhall sha256:563a7f694e18e6399f7f6d01f5b7e3c3345781655d99945768f48e458feb93a4

let overrides = {=}

let additions =
      { toppokki =
          { dependencies =
                [ "aff-promise"
                , "node-fs-aff"
                , "node-buffer"
                , "node-http"
                , "functions"
                , "record"
                , "prelude"
                ]
              : List Text
          , repo = "https://github.com/justinwoo/purescript-toppokki.git"
          , version = "v2.2.0"
          }
      , milkis =
          { dependencies =
                [ "prelude"
                , "aff-promise"
                , "typelevel-prelude"
                , "foreign-object"
                , "arraybuffer-types"
                ]
              : List Text
          , devDependencies = [ "purescript-spec" ] : List Text
          , repo = "https://github.com/justinwoo/purescript-milkis.git"
          , version = "v7.4.0"
          }
      , record-prefix =
          { dependencies =
            [ "prelude", "heterogeneous", "console", "typelevel-prelude" ]
          , repo =
              "https://github.com/dariooddenino/purescript-record-prefix.git"
          , version = "v1.0.0"
          }
      }

in  upstream ⫽ overrides ⫽ additions
