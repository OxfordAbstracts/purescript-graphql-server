module GraphQL.Server.HandleRequest where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (findMap)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (liftAff)
import GraphQL.Parser (document)
import GraphQL.Parser.AST as AST
import GraphQL.Server.GqlError (GqlError(..))
import GraphQL.Server.GqlResM (GqlResM)
import HTTPure (Request, toString)
import HTTPure.Body (RequestBody)
import Parsing (runParser)

handleRequest :: Request -> GqlResM String
handleRequest { body } = do
  doc <- getDoc body
  opDef <- maybe (throwError NoOperationDefinition) pure $ getOperationDefinition doc
  pure $ show opDef

getDoc :: RequestBody -> GqlResM AST.Document
getDoc body = do
  str <- liftAff $ toString body
  case runParser str document of
    Left err -> throwError $ CouldNotParseRequest err -- unit 
    Right doc -> pure doc

getOperationDefinition :: AST.Document -> Maybe (AST.OperationDefinition)
getOperationDefinition (AST.Document doc) = doc # findMap case _ of
  AST.Definition_ExecutableDefinition (AST.ExecutableDefinition_OperationDefinition opDef) -> Just opDef
  _ -> Nothing