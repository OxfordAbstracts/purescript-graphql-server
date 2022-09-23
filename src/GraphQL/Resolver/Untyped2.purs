module GraphQL.Resolver.Untyped2 where

-- import Prelude

-- import Data.Argonaut (Json, encodeJson, fromObject, jsonEmptyObject, jsonNull, stringify)
-- import Data.Generic.Rep (class Generic)
-- import Data.List (List(..), foldl)
-- import Data.Map (Map, lookup)
-- import Data.Maybe (Maybe(..), maybe)
-- import Data.Newtype (unwrap)
-- import Data.Traversable (for)
-- import Data.Tuple (Tuple(..))
-- import Foreign.Object as Object
-- import GraphQL.Parser.AST as AST
-- import GraphQL.Server.GqlError (ResolverError(..))
-- import Partial.Unsafe (unsafeCrashWith)
-- import Unsafe.Coerce (unsafeCoerce)

-- data Resolvers m ctx = Resolvers
--   (Map String (Map String (Resolver m ctx)))

-- data Resolver m ctx = Resolver
--   { returnType :: String
--   , get :: { args :: Json, getContext :: m ctx } -> m Json
--   }

-- data Result
--   = ResultLeaf Json
--   | ResultError ResolverError
--   | ResultObject (List (Tuple String Result))
--   | ResultList (List Result)
--   | ResultNullable (Maybe Result)

-- derive instance Generic Result _
-- derive instance Eq Result

-- instance Show Result where
--   show = case _ of
--     ResultLeaf json -> "(ResultLeaf " <> stringify json <> ")"
--     ResultError err -> "(ResultError " <> show err <> ")"
--     ResultObject fields -> "(ResultObject " <> show fields <> ")"
--     ResultList items -> "(ResultList " <> show items <> ")"
--     ResultNullable maybeResult -> "(ResultNullable " <> show maybeResult <> ")"

-- resolveSelectionSet :: forall m ctx. Applicative m => Resolvers m ctx -> m ctx -> String ->  AST.SelectionSet -> m Result
-- resolveSelectionSet  (Resolvers resolvers) getContext typeName (AST.SelectionSet selections) =
--   case getSelectionFields =<< selections, lookup typeName resolvers of
--     _, Nothing -> err NoFields
--     Nil, _ -> err NoFields
--     selectedFields, Just typeResolvers -> ResultObject <$> for selectedFields
--       \{ arguments
--        , name
--        , selectionSet
--        } -> do
--         Tuple name <$> case lookup name typeResolvers of 
--           Nothing -> pure $ ResultError FieldNotFound
--           Just (Resolver resolver@{returnType}) -> 
--             let 
--                 args = maybe jsonEmptyObject (encodeArguments <<< unwrap) arguments
--             in 
--             -- unsafeCoerce unit
--             resolveSelectionSet (Resolvers resolvers) getContext typeName (resolver.get {args, getContext}) ?d



--   -- case lookup name fields of
--   --   Nothing -> pure $ ResultError FieldNotFound
--   --   Just field ->
--   --     let
--   --       args = maybe jsonEmptyObject (encodeArguments <<< unwrap) arguments
--   --     in
--   --       resolve (field.resolver { args, parent }) selectionSet
--   where
--   getSelectionFields :: AST.Selection -> List AST.T_Field
--   getSelectionFields = case _ of
--     (AST.Selection_Field (AST.Field sf)) -> pure sf
--     _ -> Nil

--   encodeArguments :: List AST.Argument -> Json
--   encodeArguments = fromObject <<< foldl insertArg Object.empty
--     where
--     insertArg obj (AST.Argument { name, value }) = Object.insert name (encodeValue value) obj

--   encodeValue :: AST.Value -> Json
--   encodeValue = case _ of
--     AST.Value_Variable _ -> unsafeCrashWith "encode Value_Variable not implemented"
--     AST.Value_IntValue (AST.IntValue a) -> encodeJson a
--     AST.Value_FloatValue (AST.FloatValue a) -> encodeJson a
--     AST.Value_StringValue (AST.StringValue a) -> encodeJson a
--     AST.Value_BooleanValue (AST.BooleanValue a) -> encodeJson a
--     AST.Value_NullValue _ -> jsonNull
--     AST.Value_EnumValue (AST.EnumValue a) -> encodeJson a
--     AST.Value_ListValue (AST.ListValue l) -> encodeJson $ map encodeValue l
--     AST.Value_ObjectValue (AST.ObjectValue args) -> encodeArguments args

--   err = pure <<< ResultError