{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "array-builder"
  , "arraybuffer-types"
  , "arrays"
  , "atleast"
  , "bifunctors"
  , "console"
  , "control"
  , "convertable-options"
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
  , "functions"
  , "functors"
  , "halogen-subscriptions"
  , "heterogeneous"
  , "http-methods"
  , "identity"
  , "indexed-monad"
  , "integers"
  , "js-date"
  , "js-object"
  , "js-promise"
  , "js-promise-aff"
  , "js-timers"
  , "js-unsafe-stringify"
  , "lists"
  , "marlowe"
  , "maybe"
  , "monad-loops"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "orders"
  , "partial"
  , "polyform"
  , "polyform-batteries-core"
  , "polyform-batteries-urlencoded"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "qualified-do"
  , "quickcheck"
  , "random"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "refined"
  , "refs"
  , "row-joins"
  , "safe-coerce"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-eval"
  , "typelevel-prelude"
  , "undefined-is-not-a-problem"
  , "unfoldable"
  , "safe-coerce"
  , "unsafe-coerce"
  , "validation"
  , "variant"
  , "web-dom"
  , "web-encoding"
  , "web-html"
  , "web-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
