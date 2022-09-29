module GraphQL.Server.GqlResM (GqlResM(..), toAff, toResponse) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import GraphQL.Server.GqlError (GqlError(..))
import HTTPure (ResponseM, badRequest, ok)
import Parsing (parseErrorMessage)

newtype GqlResM a = GqlResM (ExceptT GqlError Aff a)

derive instance Newtype (GqlResM a) _
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
    Left (OtherError str) -> badRequest str
    Left err -> badRequest $ show err
    Right res -> ok res

toAff :: forall a. GqlResM a -> Aff (Either GqlError a)
toAff = unwrap >>> runExceptT