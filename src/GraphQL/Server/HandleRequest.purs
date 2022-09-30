module GraphQL.Server.HandleRequest where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (Json)
import Data.Either (Either(..), either)
import Data.Foldable (findMap)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Resolver.HandleQuery (handleOperationDefinition)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Server.GqlError (GqlError(..))
import GraphQL.Server.GqlResM (GqlResM)
import HTTPure (Request, toString)
import HTTPure.Body (RequestBody)
import Parsing (runParser)

handleRequest
  :: forall m
   . Applicative m
  => (m (Either GqlError Json) -> Aff (Either GqlError Json))
  -> Resolver m
  -> Request
  -> GqlResM Json
handleRequest runM resolvers req = do
  op <- parseOperation req
  either throwError pure =<< (liftAff $ runM $ handleOperationDefinition resolvers op)

parseOperation :: Request -> GqlResM AST.OperationDefinition
parseOperation { body } = do
  doc <- getDoc body
  maybe (throwError NoOperationDefinition) pure $ getOperationDefinition doc

getDoc
  :: forall m
   . MonadAff m
  => MonadThrow GqlError m
  => RequestBody
  -> m AST.Document
getDoc body = do
  str <- liftAff $ toString body
  case runParser str document of
    Left err -> throwError $ CouldNotParseRequest err
    Right doc -> pure doc

getOperationDefinition :: AST.Document -> Maybe (AST.OperationDefinition)
getOperationDefinition (AST.Document doc) = doc # findMap case _ of
  AST.Definition_ExecutableDefinition (AST.ExecutableDefinition_OperationDefinition opDef) -> Just opDef
  _ -> Nothing