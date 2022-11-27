module GraphQL.Resolver.JsonResolver
  ( Field
  , Fields
  , Resolver(..)
  , TopLevelJsonResolver
  , resolve
  , resolveQueryString
  ) where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (local)
import Control.Parallel (class Parallel, parTraverse, parallel, sequential)
import Data.Argonaut (Json, jsonEmptyObject)
import Data.Either (Either(..))
import Data.GraphQL.AST (SelectionSet)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (selectionSet)
import Data.List (List(..), (:))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver.EncodeValue (encodeArguments)
import GraphQL.Resolver.GqlM (GqlM)
import GraphQL.Resolver.Path (PathPart(..))
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Server.GqlError (GqlError(..), FailedToResolve(..))
import Parsing (runParser)
import Safe.Coerce (coerce)

type TopLevelJsonResolver =
  { query :: Resolver
  , mutation :: Maybe (Resolver)
  , subscription :: Maybe (Resolver)
  }

data Resolver
  = Node (GqlM Json)
  | ListResolver (List (Resolver))
  | Fields Fields
  | AsyncResolver (GqlM (Resolver))
  | Null
  | FailedResolver (FailedToResolve Error)

type Fields =
  { fields :: Map String (Field)
  , typename :: String
  }

type Field =
  { name :: String
  , resolver :: { args :: Json } -> Resolver
  }

resolveQueryString
  :: Resolver
  -> String
  -> GqlM (Either GqlError (Result Error))
resolveQueryString resolver query = do
  let
    queryParseResult = runParser query selectionSet
  case queryParseResult of
    Left err -> pure $ Left $ ParseGqlDocumentError err
    Right queryAST -> do
      Right <$> resolve resolver Object.empty (Just queryAST)

resolve
  :: Resolver
  -> Object Json
  -> (Maybe AST.SelectionSet)
  -> GqlM (Result Error)
resolve resolver vars = case resolver, _ of
  AsyncResolver resolverM, a -> catchError resolveAsync handleError
    where
    resolveAsync = resolverM >>= \resolver' -> resolve resolver' vars a

    handleError = pure <<< ResultError <<< ResolverError

  FailedResolver error, _ -> err error
  Node _, Just _ -> err SelectionSetAtNodeValue
  Node node, _ -> ResultLeaf <$> node
  ListResolver resolvers, selectionSet ->
    ResultList <$> gqlTraverseList vars selectionSet resolvers
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
            Just field ->
              let
                args = maybe jsonEmptyObject (encodeArguments vars <<< unwrap) arguments
                setEnv e = e
                  { depth = e.depth + 1
                  , path = Field field.name : e.path
                  }
              in
                local setEnv $
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

gqlTraverseList :: Object Json -> Maybe SelectionSet -> List Resolver -> GqlM (List (Result Error))
gqlTraverseList vars selectionSet = parTraverseWithIndex (map (flip catchError handleError) <<< go)
  where

  go i r =
    let
      setEnv env = env
        { index = Just i
        , path = Index i : env.path
        }
    in
      local setEnv $ resolve r vars selectionSet

  handleError = pure <<< ResultError <<< ResolverError

parTraverseWithIndex
  :: forall f m t a b i
   . Parallel f m
  => TraversableWithIndex i t
  => (i -> a -> m b)
  -> t a
  -> m (t b)
parTraverseWithIndex f = sequential <<< traverseWithIndex (map parallel <<< f)
