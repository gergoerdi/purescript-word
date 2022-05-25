{ name = "word"
, dependencies =
  [ "assert"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "numerics"
  , "prelude"
  , "rationals"
  , "strings"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
