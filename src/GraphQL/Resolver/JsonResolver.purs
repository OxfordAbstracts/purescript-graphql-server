module GraphQL.Resolver.JsonResolver where

import Prelude

import Data.Argonaut (Json, jsonEmptyObject)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (selectionSet)
import Data.List (List(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver.EncodeValue (encodeArguments)
import GraphQL.Resolver.Gqlable (class Gqlable, gqlParallel, gqlSequential)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Server.GqlError (GqlError(..), ResolverError(..))
import Parsing (runParser)
import Safe.Coerce (coerce)

type TopLevelJsonResolver m =
  { query :: Resolver m
  , mutation :: Maybe (Resolver m)
  , subscription :: Maybe (Resolver m)
  }

data Resolver m
  = Node (m Json)
  | ListResolver (List (Resolver m))
  | Fields (Fields m)
  | ResolveAsync (m (Resolver m))
  | NullableResolver (Maybe (Resolver m))
  | FailedResolver ResolverError

type Fields m =
  { fields :: Map String (Field m)
  , typename :: String
  }

type Field m =
  { name :: String
  , resolver :: { args :: Json } -> Resolver m
  }

resolveQueryString
  :: forall f m
   . Gqlable f m
  => Resolver f
  -> String
  -> f (Either GqlError Result)
resolveQueryString resolver query = do
  let
    queryParseResult = runParser query selectionSet
  case queryParseResult of
    Left err -> pure $ Left $ ParseGqlDocumentError err
    Right queryAST -> do
      Right <$> resolve resolver Object.empty (Just queryAST)

resolve
  :: forall f m
   . Applicative f
  => Monad m
  => Gqlable f m
  => Resolver f
  -> Object Json
  -> (Maybe AST.SelectionSet)
  -> f Result
resolve resolver vars = case resolver, _ of
  ResolveAsync resolverM, a -> gqlParallel do
    resolver' <- gqlSequential resolverM
    gqlSequential $ resolve resolver' vars a
  FailedResolver error, _ -> err error
  Node _, Just _ -> err SelectionSetAtNodeValue
  Node node, _ -> ResultLeaf <$> node
  ListResolver resolvers, selectionSet ->
    ResultList <$> traverse (\r -> resolve r vars selectionSet) resolvers
  NullableResolver resolvers, selectionSet ->
    ResultNullable <$> traverse (\r -> resolve r vars selectionSet) resolvers
  Fields _, Nothing -> err MissingSelectionSet
  (Fields { fields, typename }), Just (AST.SelectionSet selections) ->
    case getSelectionFields typename =<< selections of
      Nil -> err NoFields
      selectedFields -> ResultObject <$> for selectedFields
        \{ arguments
         , name
         , selectionSet
         } -> do
          Tuple name <$> case lookup name fields of
            Nothing -> pure $ ResultError FieldNotFound
            Just field ->
              let
                args = maybe jsonEmptyObject (encodeArguments vars <<< unwrap) arguments
              in
                resolve (field.resolver { args }) vars selectionSet
  where
  getSelectionFields :: String -> AST.Selection -> List AST.T_Field
  getSelectionFields typename = case _ of
    AST.Selection_Field (AST.Field sf) -> pure sf
    AST.Selection_InlineFragment
      ( AST.InlineFragment
          { typeCondition: Just typeCondition
          , selectionSet: (AST.SelectionSet selectionSet)
          }
      ) | coerce typename == typeCondition ->
      getSelectionFields typename =<< selectionSet
    AST.Selection_InlineFragment
      ( AST.InlineFragment
          { typeCondition: Nothing
          , selectionSet: (AST.SelectionSet selectionSet)
          }
      ) ->
      getSelectionFields typename =<< selectionSet
    _ -> Nil

  err = pure <<< ResultError
