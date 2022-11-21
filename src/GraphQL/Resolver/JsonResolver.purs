module GraphQL.Resolver.JsonResolver where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Parallel (class Parallel, parTraverse)
import Data.Argonaut (Json, jsonEmptyObject)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (selectionSet)
import Data.List (List(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver.EncodeValue (encodeArguments)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Server.GqlError (GqlError(..), FailedToResolve(..))
import Parsing (runParser)
import Safe.Coerce (coerce)

type TopLevelJsonResolver err m =
  { query :: Resolver err m
  , mutation :: Maybe (Resolver err m)
  , subscription :: Maybe (Resolver err m)
  }

data Resolver err m
  = Node (m Json)
  | ListResolver (List (Resolver err m))
  | Fields (Fields err m)
  | AsyncResolver (m (Resolver err m))
  | NullableResolver (Maybe (Resolver err m))
  | FailedResolver (FailedToResolve err)

type AffResolver = Resolver Error (GqlIo Aff)

type Fields err m =
  { fields :: Map String (Field err m)
  , typename :: String
  }

type Field err m =
  { name :: String
  , resolver :: { args :: Json } -> Resolver err m
  }

resolveQueryString
  :: forall m err f
   . MonadError err m
  => Parallel f m
  => Resolver err m
  -> String
  -> m (Either GqlError (Result err))
resolveQueryString resolver query = do
  let
    queryParseResult = runParser query selectionSet
  case queryParseResult of
    Left err -> pure $ Left $ ParseGqlDocumentError err
    Right queryAST -> do
      Right <$> resolve resolver Object.empty (Just queryAST)

resolve
  :: forall m err f
   . MonadError err m
  => Parallel f m
  => Resolver err m
  -> Object Json
  -> (Maybe AST.SelectionSet)
  -> m (Result err)
resolve resolver vars = case resolver, _ of
  AsyncResolver resolverM, a -> catchError resolveAsync handleError
    where
    resolveAsync = resolverM >>= \resolver' -> resolve resolver' vars a

    handleError = pure <<< ResultError <<< ResolverError

  FailedResolver error, _ -> err error
  Node _, Just _ -> err SelectionSetAtNodeValue
  Node node, _ -> ResultLeaf <$> node
  ListResolver resolvers, selectionSet ->
    ResultList <$> parTraverse (\r -> resolve r vars selectionSet) resolvers
  NullableResolver resolvers, selectionSet ->
    ResultNullable <$> parTraverse (\r -> resolve r vars selectionSet) resolvers
  Fields _, Nothing -> err MissingSelectionSet
  (Fields { fields, typename }), Just (AST.SelectionSet selections) ->
    case getSelectionFields typename =<< selections of
      Nil -> err NoFields
      selectedFields -> ResultObject <$> flip parTraverse selectedFields
        \{ arguments
         , name
         , alias
         , selectionSet
         } -> do
          Tuple (fromMaybe name alias) <$> case lookup name fields of
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
