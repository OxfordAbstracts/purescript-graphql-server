module GraphQL.Server.GqlResM (GqlResM(..), toResponse) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import GraphQL.Server.GqlError (GqlError(..))
import HTTPure (ResponseM, badRequest, ok)
import Parsing (parseErrorMessage)

newtype GqlResM a = GqlResM (ExceptT GqlError Aff a)

derive instance Functor GqlResM
derive newtype instance Apply GqlResM
derive newtype instance Bind GqlResM
derive newtype instance Applicative GqlResM
derive newtype instance Monad GqlResM
derive newtype instance MonadThrow GqlError GqlResM
derive newtype instance MonadEffect GqlResM
derive newtype instance MonadAff GqlResM


toResponse :: GqlResM String -> ResponseM
toResponse (GqlResM gqlResM) = do
  e <- runExceptT gqlResM
  case e of
    Left (CouldNotParseRequest err) -> badRequest $ parseErrorMessage err
    Left NoOperationDefinition -> badRequest "No operation definition in request body"
    Right res -> ok res