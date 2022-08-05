module GraphQL.Server.Resolver where


import Control.Monad.Except (ExceptT)
import Data.Argonaut (Json)
import Effect.Aff (Aff)
import GraphQL.Parser.AST as AST

data ResolverError = ResolverError String

newtype ResolverM a = ResolverM (ExceptT ResolverError Aff a)

class Resolver schema resolver where
  resolve :: schema -> resolver -> AST.OperationDefinition -> ResolverM Json


