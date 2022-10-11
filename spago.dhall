{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "graphql-parser"
  , "heterogeneous"
  , "httpure"
  , "identity"
  , "integers"
  , "invariant"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-http"
  , "node-streams"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "parsing"
  , "partial"
  , "prelude"
  , "record"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
