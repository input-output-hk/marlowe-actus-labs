{ name = "my-project"
, dependencies =
  [ "argonaut", "argonaut-codecs", "argonaut-generic", "console", "effect", "either", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
