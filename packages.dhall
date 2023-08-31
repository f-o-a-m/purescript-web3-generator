let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd

let overrides =
  { language-cst-parser =
      (upstream.language-cst-parser with version = "v0.13.0")
  }

let additions =
      { bytestrings =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "leibniz"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "quotient"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/rightfold/purescript-bytestrings"
        , version = "6733a32fca306015b3428e9985ffac65325a9864"
        }
      , web3 =
        { dependencies =
          [ "aff"
          , "coroutines"
          , "coroutine-transducers"
          , "effect"
          , "errors"
          , "eth-core"
          , "foreign"
          , "fork"
          , "heterogeneous"
          , "parsing"
          , "partial"
          , "profunctor-lenses"
          , "tagged"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          , "argonaut"
          , "argonaut-generic"
          , "arrays"
          , "bifunctors"
          , "bytestrings"
          , "control"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "maybe"
          , "newtype"
          , "parallel"
          , "prelude"
          , "record"
          , "ring-modules"
          , "simple-json"
          , "strings"
          , "tailrec"
          , "tuples"
          , "unfoldable"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3"
        , version = "f00f"
        -- , version = "f19ed33bc25a6e1701801e3498f1636980cfb90f"
        }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "arrays"
          , "bytestrings"
          , "effect"
          , "either"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "node-buffer"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quotient"
          , "ring-modules"
          , "simple-json"
          , "strings"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version = "master"
        }
      , coroutine-transducers =
        { dependencies =
            [ "aff"
            , "coroutines"
            , "effect"
            , "maybe"
            , "psci-support"
            ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version =
            "v1.0.0"
        }
      , errors =
        { dependencies =
          [ "control"
          , "effect"
          , "either"
          , "identity"
          , "maybe"
          , "newtype"
          , "prelude"
          , "test-unit"
          , "transformers"
          ]
        , repo = "https://github.com/passy/purescript-errors"
        , version = "670485beb1e026f77d52ca58ce10c145d96c11ba"
        }
      , mkdirp =
        { dependencies =
            [ "effect"
            , "either"
            , "exceptions"
            , "functions"
            , "node-fs"
            , "nullable"
            , "prelude"
            ]
        , repo =
            "https://github.com/f-o-a-m/purescript-mkdirp"
        , version =
            "v2.0.0"
        }
      , tagged =
        { dependencies =
            [ "identity"
            , "profunctor"
            ]
        , repo =
            "https://github.com/LiamGoodacre/purescript-tagged"
        , version =
            "v4.0.2"
        }
      , quotient =
        { dependencies = [ "prelude", "quickcheck" ]
        , repo = "https://github.com/rightfold/purescript-quotient.git"
        , version = "v3.0.0"
        }
      , dodo-printer =
        { dependencies =
          [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.1.0"
        }
      , tidy =
        { dependencies =
          [ "arrays"
          , "control"
          , "dodo-printer"
          , "either"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "language-cst-parser"
          , "strings"
          , "tuples"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy.git"
        , version = "v0.10.0"
        }
      , tidy-codegen =
        { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "console"
          , "control"
          , "dodo-printer"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "filterable"
          , "foldable-traversable"
          , "free"
          , "identity"
          , "integers"
          , "language-cst-parser"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-child-process"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-streams"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "record"
          , "safe-coerce"
          , "st"
          , "strings"
          , "tidy"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "unicode"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-tidy-codegen.git"
        , version = "v4.0.0"
        }
      }

in  upstream // overrides // additions
