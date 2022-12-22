{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "array-builder"
  , "arrays"
  , "atleast"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "decimals"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "fetch"
  , "fetch-argonaut"
  , "fetch-core"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "heterogeneous"
  , "http-methods"
  , "indexed-monad"
  , "integers"
  , "js-date"
  , "js-object"
  , "js-promise-aff"
  , "js-timers"
  , "lists"
  , "marlowe"
  , "maybe"
  , "monad-loops"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "nullable"
  , "ordered-collections"
  , "orders"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "quickcheck"
  , "random"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "refined"
  , "refs"
  , "row-joins"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "typelevel-eval"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "web-dom"
  , "web-encoding"
  , "web-html"
  , "web-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
