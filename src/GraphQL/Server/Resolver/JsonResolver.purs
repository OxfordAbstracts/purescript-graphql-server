module GraphQL.Server.Resolver.JsonResolver
  ( Field
  , Fields
  , Resolver(..)
  , TopLevelJsonResolver
  , resolve
  , resolveQueryString
  ) where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ask, local)
import Control.Parallel (class Parallel, parTraverse, parallel, sequential)
import Data.Argonaut (Json, jsonEmptyObject)
import Data.Either (Either(..))
import Data.GraphQL.AST (SelectionSet)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (selectionSet)
import Data.Lazy (Lazy)
import Data.List (List(..), (:))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import GraphQL.Server.EncodeValue (encodeArguments)
import GraphQL.Server.GqlError (GqlError(..), FailedToResolve(..))
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Resolver.Path (PathPart(..))
import GraphQL.Server.Resolver.Result (Result(..))
import Parsing (runParser)
import Safe.Coerce (coerce)

type TopLevelJsonResolver env =
  { query :: Resolver env
  , mutation :: Maybe (Resolver env)
  , subscription :: Maybe (Resolver env)
  }

data Resolver env
  = Node (GqlM env Json)
  | ListResolver (List (Resolver env))
  | Fields (Fields env)
  | AsyncResolver (GqlM env (Resolver env))
  | Null
  | FailedResolver (FailedToResolve Error)

type Fields env =
  { fields :: Map String (Field env)
  , typename :: String
  }

type Field env =
  { name :: String
  , resolver :: { args :: Json } -> Resolver env
  }

resolveQueryString
  :: forall env
   . Resolver env
  -> String
  -> GqlM env (Either GqlError (Result Error))
resolveQueryString resolver query = do
  let
    queryParseResult = runParser query selectionSet
  case queryParseResult of
    Left err -> pure $ Left $ ParseGqlDocumentError err
    Right queryAST -> do
      Right <$> resolve resolver (Just queryAST)

resolve
  :: forall env
   . Resolver env
  -> (Maybe AST.SelectionSet)
  -> GqlM env (Result Error)
resolve resolver = case resolver, _ of
  AsyncResolver resolverM, a -> catchError resolveAsync handleError
    where
    resolveAsync = resolverM >>= \resolver' -> resolve resolver' a

    handleError = pure <<< ResultError <<< ResolverError

  FailedResolver error, _ -> err error
  Node _, Just _ -> err SelectionSetAtNodeValue
  Node node, _ -> ResultLeaf <$> node
  ListResolver resolvers, selectionSet -> do
    ResultList <$> gqlTraverseList selectionSet resolvers
  Null, _ -> pure ResultNull
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
            Just field -> do
              { variables } <- ask
              let
                args = maybe jsonEmptyObject (encodeArguments variables <<< unwrap) arguments
                setEnv e = e
                  { depth = e.depth + 1
                  , path = Field field.name : e.path
                  }
              local setEnv $
                resolve (field.resolver { args }) selectionSet
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

gqlTraverseList :: forall env. Maybe SelectionSet -> List (Resolver env) -> GqlM env (List (Result Error))
gqlTraverseList selectionSet = parTraverseWithIndex (map (flip catchError handleError) <<< go)
  where

  go i r =
    let
      setEnv env = env
        { index = Just i
        , path = Index i : env.path
        }
    in
      local setEnv $ resolve r selectionSet

  handleError = pure <<< ResultError <<< ResolverError

parTraverseWithIndex
  :: forall f m t a b i
   . Parallel f m
  => TraversableWithIndex i t
  => (i -> a -> m b)
  -> t a
  -> m (t b)
parTraverseWithIndex f = sequential <<< traverseWithIndex (map parallel <<< f)
