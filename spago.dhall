{ name = "my-project"
, dependencies =
  [ "argonaut-generic", "console", "effect", "marlowe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
