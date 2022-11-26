let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221122/packages.dhall
        sha256:c0cfb849a0f886d38cab7bbf40a5c23911d82ba4f09946cbc9d7b5362f2b8819

in  upstream

  with classless =
      { dependencies =
          [ "heterogeneous", "prelude", "record"
          ]
      , repo =
          "https://github.com/thought2/purescript-classless.git"
      , version =
          "main"
      }