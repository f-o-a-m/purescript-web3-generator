let conf = ./spago.dhall

in    conf
    ⫽ { sources = conf.sources # [ "contracts/**/*.purs"  ]
      , dependencies =
            conf.dependencies
          # [ "arrays"
            , "aff"
            , "control"
            , "argonaut-codecs"
            , "argonaut-core"
            , "argonaut-traversals"
            , "bifunctors"
            , "either"
            , "exceptions"
            , "foldable-traversable"
            , "integers"
            , "lists"
            , "maybe"
            , "node-buffer"
            , "node-fs"
            , "node-path"
            , "nonempty"
            , "partial"
            , "profunctor-lenses"
            , "strings"
            , "tagged"
            , "transformers"
            , "tuples"
            , "web3"
            , "newtype"
            ]
      }
