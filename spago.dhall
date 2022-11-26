{ name = "classless-decode-json"
, dependencies =
  [ "argonaut"
  , "classless"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
