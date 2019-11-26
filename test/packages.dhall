let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191110/packages.dhall sha256:563a7f694e18e6399f7f6d01f5b7e3c3345781655d99945768f48e458feb93a4

let overrides = {=}

let additions = 
      { http =
          { dependencies = [] : List Text
          , repo = "ssh://git@github.com/joneshf/purescript-http.git"
          , version = "v4.0.0"
          },
        toppokki =
          { dependencies =
            [ "aff-promise"
            , "node-fs-aff"
            , "node-buffer"
            , "node-http"
            , "functions"
            , "record"
            , "prelude"
            ] : List Text
          , repo = "ssh://git@github.com/justinwoo/purescript-toppokki.git"
          , version = "v2.2.0"
          }
      }


in  upstream // overrides // additions
