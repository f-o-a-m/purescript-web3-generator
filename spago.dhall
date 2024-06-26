{ name = "web3-generator"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-traversals"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "eth-core"
  , "exceptions"
  , "fixed-points"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "language-cst-parser"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "optparse"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "string-parsers"
  , "strings"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
