{ name = "nftree-eco"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "maybe"
  , "milkis"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "transformers"
  , "type-equality"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
