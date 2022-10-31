module GraphQL.Resolver.Error where

import Prelude

import Effect.Exception (Error, message)


-- How errors should be displayed to api clients
class CustomResolverError err where
  renderError :: err -> String

instance CustomResolverError String where
  renderError = identity

instance CustomResolverError Error where
  renderError = message
