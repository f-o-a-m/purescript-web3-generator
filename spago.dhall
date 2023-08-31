{-
Welcome to a Spago project!
You can edit this file as you like.
-}
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
  , "errors"
  , "eth-core"
  , "exceptions"
  , "fixed-points"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "language-cst-parser"
  , "language-cst-codegen"
  , "mkdirp"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "ordered-collections"
  , "optparse"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
