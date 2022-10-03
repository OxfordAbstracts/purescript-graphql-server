module GraphQL.Resolver.JsonResolver where

import Prelude

import Control.Parallel (parallel)
import Data.Argonaut (Json, encodeJson, fromObject, jsonEmptyObject, jsonNull)
import Data.Either (Either(..))
import Data.GraphQL.AST (Selection(..), SelectionSet(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (selectionSet)
import Data.List (List(..), foldl)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import GraphQL.Resolver.GqlSequential (class GqlSequential, gqlParallel, gqlSequential)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Server.GqlError (GqlError(..), ResolverError(..))
import Parsing (runParser)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

data Resolver m
  = Node (m Json)
  | ListResolver (List (Resolver m))
  | ListResolverAsync (m (List (Resolver m)))
  | Fields (Fields m)
  | ResolveAsync (m (Resolver m))
  | FailedResolver ResolverError

type Fields m =
  { fields :: Map String (Field m)
  }

type Field m =
  { name :: String
  , resolver :: { args :: Json } -> Resolver m
  }

resolveQueryString
  :: forall f m
   . GqlSequential f m
  => Resolver f
  -> String
  -> f (Either GqlError Result)
resolveQueryString resolver query = do
  let
    queryParseResult = runParser query selectionSet
  case queryParseResult of
    Left err -> pure $ Left $ CouldNotParseRequest err
    Right queryAST -> do
      Right <$> resolve resolver (Just queryAST)

resolve
  :: forall f m
   . Applicative f
  => Monad m
  => GqlSequential f m
  => Resolver f
  -> (Maybe SelectionSet)
  -> f Result
resolve = case _, _ of
  ResolveAsync resolverM, a -> gqlParallel do 
    resolver <- gqlSequential resolverM
    gqlSequential $ resolve resolver a
  FailedResolver error, _ -> err error
  Node _, Just _ -> err SelectionSetAtNodeValue
  Node node, _ -> ResultLeaf <$> node
  _, Nothing -> err MissingSelectionSet
  ListResolver resolvers, selectionSet ->
    ResultList <$> traverse (\r -> resolve r selectionSet) resolvers
  ListResolverAsync resolverM, selectionSet -> gqlParallel do
    resolvers <- gqlSequential resolverM
    gqlSequential $ ResultList <$> (traverse (\r -> resolve r selectionSet) resolvers)
  (Fields { fields }), Just (SelectionSet selections) ->
    case getSelectionFields =<< selections of
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
                args = maybe jsonEmptyObject (encodeArguments <<< unwrap) arguments
              in
                resolve (field.resolver { args }) selectionSet
  where
  getSelectionFields :: Selection -> List AST.T_Field
  getSelectionFields = case _ of
    (Selection_Field (AST.Field sf)) -> pure sf
    _ -> Nil

  encodeArguments :: List AST.Argument -> Json
  encodeArguments = fromObject <<< foldl insertArg Object.empty
    where
    insertArg obj (AST.Argument { name, value }) = Object.insert name (encodeValue value) obj

  encodeValue :: AST.Value -> Json
  encodeValue = case _ of
    AST.Value_Variable _ -> unsafeCrashWith "encode Value_Variable not implemented"
    AST.Value_IntValue (AST.IntValue a) -> encodeJson a
    AST.Value_FloatValue (AST.FloatValue a) -> encodeJson a
    AST.Value_StringValue (AST.StringValue a) -> encodeJson a
    AST.Value_BooleanValue (AST.BooleanValue a) -> encodeJson a
    AST.Value_NullValue _ -> jsonNull
    AST.Value_EnumValue (AST.EnumValue a) -> encodeJson a
    AST.Value_ListValue (AST.ListValue l) -> encodeJson $ map encodeValue l
    AST.Value_ObjectValue (AST.ObjectValue args) -> encodeArguments args

  err = pure <<< ResultError

  -- resolveNode = case _ of
  --   NodeJson json -> pure json
  --   NodeAsync async -> gqlParallel do
  --     n <- gqlSequential async
  --     gqlSequential $ resolveNode n