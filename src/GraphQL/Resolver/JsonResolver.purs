module GraphQL.Resolver.JsonResolver where

import Prelude

import Data.Argonaut (Json, encodeJson, fromObject, jsonEmptyObject, jsonNull, stringify)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), foldl)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import GraphQL.Parser (selectionSet)
import GraphQL.Parser.AST (Selection(..), SelectionSet(..))
import GraphQL.Parser.AST as AST
import GraphQL.Server.GqlError (GqlError(..), ResolverError(..))
import Parsing (runParser)
import Partial.Unsafe (unsafeCrashWith)

data Resolver m
  = Node (m Json)
  | Fields (Fields m)
  | FailedResolver ResolverError

type Fields m =
  { fields :: Map String (Field m)
  }

type Field m =
  { name :: String
  , args :: Args
  , resolver :: { args :: Json } -> Resolver m
  }

type Args = List { name :: String, type :: String, required :: Boolean }

data Result
  = ResultLeaf Json
  | ResultError ResolverError
  | ResultObject (List (Tuple String Result))
  | ResultList (List Result)
  | ResultNullable (Maybe Result)

derive instance Generic Result _
derive instance Eq Result

instance Show Result where
  show = case _ of
    ResultLeaf json -> "(ResultLeaf " <> stringify json <> ")"
    ResultError err -> "(ResultError " <> show err <> ")"
    ResultObject fields -> "(ResultObject " <> show fields <> ")"
    ResultList items -> "(ResultList " <> show items <> ")"
    ResultNullable maybeResult -> "(ResultNullable " <> show maybeResult <> ")"

resolveQueryString :: forall m. Applicative m => Resolver m -> String -> m (Either GqlError Result)
resolveQueryString resolver query = do
  let
    queryParseResult = runParser query selectionSet
  case queryParseResult of
    Left err -> pure $ Left $ CouldNotParseRequest err
    Right queryAST -> do
      Right <$> resolve resolver (Just queryAST)

resolve
  :: forall m
   . Applicative m
  => Resolver m
  -> (Maybe SelectionSet)
  -> m Result
resolve = case _, _ of
  FailedResolver error, _ -> err error
  Node _, Just _ -> err SelectionSetAtNodeValue
  Node node, _ -> ResultLeaf <$> node
  _, Nothing -> err MissingSelectionSet
  -- ResolveList r,   -> err SelectionSetAtListValue
  (Fields { fields }), Just (SelectionSet selections) -> do
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