let mkPackage =
  https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221201/packages.dhall
    sha256:d1a68fa15709eaa686515eb5b9950d82c743f7bf73e3d87a4abe9e1be6fda571

in upstream
  with
    json-helpers = mkPackage
      [ "aff"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "contravariant"
      , "control"
      , "effect"
      , "either"
      , "enums"
      , "foldable-traversable"
      , "foreign-object"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor"
      , "psci-support"
      , "quickcheck"
      , "record"
      , "spec"
      , "spec-quickcheck"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      ]
      "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
      "60615c36abaee16d8dbe09cdd0e772e6d523d024"
  with
    marlowe = mkPackage
      [ "argonaut"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "bigints"
      , "contravariant"
      , "control"
      , "datetime"
      , "effect"
      , "either"
      , "enums"
      , "foldable-traversable"
      , "foreign-object"
      , "functions"
      , "integers"
      , "json-helpers"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "profunctor-lenses"
      , "strings"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      , "unfoldable"
      ]
      "https://github.com/input-output-hk/purescript-marlowe.git"
      "scp-5007"
  with typelevel-eval =mkPackage
    [ "bifunctors"
    , "console"
    , "contravariant"
    , "effect"
    , "leibniz"
    , "psci-support"
    , "profunctor"
    , "record"
    , "typelevel-prelude"
    , "tuples"
    ]
    "https://github.com/natefaubion/purescript-typelevel-eval"
    "v0.5.0"
  with
    row-joins = mkPackage
      [ "console", "effect", "prelude", "typelevel-eval" ]
      "https://github.com/paluh/purescript-row-joins.git"
      "c129c7531a0ccf2b82ac1e57ab796b3df17614ff"
  with
    refined = mkPackage
      [ "argonaut", "effect", "prelude", "quickcheck", "typelevel" ]
      "https://github.com/danieljharvey/purescript-refined.git"
      "v1.0.0"
  with
    js-unsafe-stringify = mkPackage
      ([] : List Text)
      "https://github.com/paluh/purescript-js-unsafe-stringify.git"
      "v0.2.1"
  with
    js-object = mkPackage
      [ "aff", "effect", "heterogeneous", "prelude", "typelevel-prelude", "contravariant", "newtype", "record", "unsafe-coerce" ]
      "https://github.com/purescript-codegen/purescript-js-object.git"
      "73db55f89744b032f44c9ec49804f46e3ee63ed7"
  with
    atleast = ./.tmp/purescript-atleast/spago.dhall as Location
    -- atleast = mkPackage
    --   [ "arrays" , "effect" , "enums" , "fast-vect" , "foldable-traversable" , "integers" 
    --   , "maybe" , "partial" , "prelude" , "quickcheck" , "unsafe-coerce"
    --   ]
    --   "https://github.com/JamieBallingall/purescript-atleast.git"
    --   "x0.5.1"
  with
    polyform =
      mkPackage
      [ "js-unsafe-stringify", "newtype" ,"ordered-collections"
      , "variant", "profunctor", "invariant", "foreign-object"
      , "run", "transformers","validation", "foreign"
      ]
      "https://github.com/purescript-polyform/polyform.git"
      "v0.9.2"

  with
    polyform-batteries-core = mkPackage
      [ "debug", "decimals", "filterable", "numbers"
      , "polyform", "prelude", "record-extra", "test-unit"
      ]
      "https://github.com/purescript-polyform/batteries-core.git"
      "v0.3.0"
  with
    polyform-batteries-urlencoded =
      mkPackage
        [ "argonaut" , "console" , "debug" , "effect" , "form-urlencoded"
        , "polyform-batteries-core" , "psci-support" , "spec"
        ]
        "https://github.com/purescript-polyform/batteries-urlencoded.git"
        "v0.4.1"
  with datetime-iso =
      mkPackage
        [ "aff"
        , "argonaut"
        , "argonaut-codecs"
        , "argonaut-core"
        , "arrays"
        , "bifunctors"
        , "datetime"
        , "effect"
        , "either"
        , "enums"
        , "foldable-traversable"
        , "maybe"
        , "newtype"
        , "parsing"
        , "partial"
        , "prelude"
        , "spec"
        , "strings"
        , "transformers"
        ]
        "https://github.com/paluh/purescript-datetime-iso"
        "c7b75bb16e0543687fc4c9b21c34a49cdaa22f0c"
