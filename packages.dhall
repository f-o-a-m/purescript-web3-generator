let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754

let eth-core-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-eth-core/master/packages.dhall
        sha256:af2751772a729d58edf7056805007934e3687b3079f8a02ac514e705aeab8c42

let web3-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-web3/master/packages.dhall
        sha256:7195a7362d333592064c837635aae06a0092e7ff7e7eec84746eabc0929e20ba

let additions =
      { bytestrings = eth-core-deps.bytestrings
      , coroutine-transducers = web3-deps.coroutine-transducers
      , dodo-printer =
          { dependencies =
            [ "aff"
            , "ansi"
            , "arrays"
            , "avar"
            , "console"
            , "control"
            , "effect"
            , "either"
            , "exceptions"
            , "foldable-traversable"
            , "integers"
            , "lists"
            , "maybe"
            , "minibench"
            , "newtype"
            , "node-buffer"
            , "node-child-process"
            , "node-path"
            , "node-process"
            , "node-streams"
            , "parallel"
            , "partial"
            , "posix-types"
            , "prelude"
            , "safe-coerce"
            , "strings"
            , "tuples"
            ]
          , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
          , version = "v2.2.1"
          }
      , eth-core = web3-deps.eth-core
      , quotient = eth-core-deps.quotient
      , tidy =
          { dependencies =
            [ "arrays"
            , "dodo-printer"
            , "foldable-traversable"
            , "lists"
            , "maybe"
            , "ordered-collections"
            , "partial"
            , "prelude"
            , "language-cst-parser"
            , "strings"
            , "tuples"
            ]
          , repo = "https://github.com/natefaubion/purescript-tidy.git"
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
      , web3 = 
        {  dependencies =
             [ "aff"
             , "argonaut"
             , "arrays"
             , "bifunctors"
             , "bytestrings"
             , "control"
             , "coroutine-transducers"
             , "coroutines"
             , "effect"
             , "either"
             , "eth-core"
             , "exceptions"
             , "foldable-traversable"
             , "foreign"
             , "foreign-object"
             , "fork"
             , "gen"
             , "heterogeneous"
             , "maybe"
             , "newtype"
             , "parallel"
             , "parsing"
             , "partial"
             , "prelude"
             , "profunctor-lenses"
             , "record"
             , "ring-modules"
             , "simple-json"
             , "strings"
             , "tagged"
             , "tailrec"
             , "transformers"
             , "tuples"
             , "typelevel-prelude"
             , "unfoldable"
             , "unsafe-coerce"
             , "variant"
             , "identity"
             ],
           repo = "https://github.com/f-o-a-m/purescript-web3.git",
           version = "v7.2.0"
        }
      }

in  upstream // additions
