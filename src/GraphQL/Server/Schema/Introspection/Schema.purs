module GraphQL.Server.Schema.Introspection.Schema (IntrospectionError(..), introspectSchema) where

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.List (List(..), find, findMap, mapMaybe)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import GraphQL.Resolver.Root (GqlRoot)
import GraphQL.Server.Schema.GetDocument (class GetDocument, getDocument, getSchemaDefinition)
import GraphQL.Server.Schema.Introspection.TypeDefinitionGetter (getTypeDefDescription, getTypeDefFields, getTypeDefKind, getTypeDefName, getTypeDefPossibleValues)
import GraphQL.Server.Schema.Introspection.TypeName (class GqlTypeName)
import GraphQL.Server.Schema.Introspection.Types as ITypes
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data IntrospectionError
  = NamedTypeNotDefined String
  | NoQueryType

derive instance Generic IntrospectionError _

derive instance Eq IntrospectionError

instance showIntrospectionError :: Show IntrospectionError where
  show = genericShow

type Err = Either (NonEmptyList IntrospectionError)

type Env = { namedTypes :: Map String AST.TypeDefinition }

introspectSchema
  :: forall root
   . GetDocument root
  => root
  -> Err ITypes.ISchema
introspectSchema root = do
  types <- join <$> traverse (definitionToIType env) defs
  queryName <- note (pure NoQueryType) $ defs # findMap getQueryType
  queryTypeAst <- lookupType env queryName
  queryType <- typeDefinitionToIType env queryTypeAst
  pure $ ITypes.ISchema
    { directives: mempty
    , queryType
    , mutationType: Nothing
    , subscriptionType: Nothing
    , types
    }
  where
  (AST.Document defs) = getDocument root

  typeDefs :: List AST.TypeDefinition
  typeDefs = mapMaybe defintionToTypeDefinition defs

  env :: Env
  env = { namedTypes: Map.fromFoldable $ typeDefs <#> \def -> Tuple (getTypeDefName def) def }

  getQueryType :: AST.Definition -> Maybe AST.NamedType
  getQueryType = case _ of
    AST.Definition_TypeSystemDefinition
      ( AST.TypeSystemDefinition_SchemaDefinition
          ( AST.SchemaDefinition
              { rootOperationTypeDefinition }
          )
      ) ->
      findMap getRootDef rootOperationTypeDefinition
    _ -> Nothing

    where 
    getRootDef :: AST.RootOperationTypeDefinition -> Maybe AST.NamedType
    getRootDef = unsafeCoerce

defintionToTypeDefinition :: AST.Definition -> Maybe AST.TypeDefinition
defintionToTypeDefinition = case _ of
  AST.Definition_TypeSystemDefinition
    ( AST.TypeSystemDefinition_TypeDefinition
        d
    ) -> pure d
  _ -> Nothing

definitionToIType :: Env -> AST.Definition -> Err (List ITypes.IType)
definitionToIType env = case _ of
  AST.Definition_TypeSystemDefinition
    ( AST.TypeSystemDefinition_TypeDefinition
        d
    ) -> map pure $ typeDefinitionToIType env d
  _ -> pure Nil

typeDefinitionToIType :: Env -> AST.TypeDefinition -> Err ITypes.IType
typeDefinitionToIType env = case _ of
  AST.TypeDefinition_ScalarTypeDefinition (AST.ScalarTypeDefinition d) -> do
    pure $ ITypes.IType
      { kind: ITypes.SCALAR
      , name: Just d.name
      , description: d.description
      , fields: \_ -> Nothing
      , interfaces: Nothing
      , possibleTypes: Nothing
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Nothing
      }
  AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition d) -> do
    fields <- fieldsDefinitionToIFields env d.fieldsDefinition
    -- d.fieldsDefinition <#> unwrap # traverse (traverse (fieldDefinitionToIField env))
    pure $ ITypes.IType
      { kind: ITypes.OBJECT
      , name: Just d.name
      , description: d.description
      , fields: \{} -> fields
      , interfaces: Nothing
      , possibleTypes: Nothing
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Nothing
      }
  AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition d) -> do
    fields <- fieldsDefinitionToIFields env d.fieldsDefinition
    pure $ ITypes.IType
      { kind: ITypes.INTERFACE
      , name: Just d.name
      , description: d.description
      , fields: \{} -> fields
      , interfaces: Nothing
      , possibleTypes: Nothing
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Nothing
      }
  AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition d) -> do 
    possibleTypes <- traverse (traverse (lookupType env >=> typeDefinitionToIType env)) (map unwrap d.unionMemberTypes)
    pure $ ITypes.IType
      { kind: ITypes.UNION
      , name: Just d.name
      , description: d.description
      , fields: \{} -> Nothing
      , interfaces: Nothing
      , possibleTypes
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Nothing
      }
  -- AST.TypeDefinition_EnumTypeDefinition (AST.EnumTypeDefinition d) ->
  --    ?d d
  -- AST.TypeDefinition_InputObjectTypeDefinition (AST.InputObjectTypeDefinition d) ->
  --    ?d d
  _ -> todo


-- enumType

fieldsDefinitionToIFields :: Env -> Maybe AST.FieldsDefinition -> Err (Maybe (List ITypes.IField))
fieldsDefinitionToIFields env fieldsDefinition = 
  fieldsDefinition <#> unwrap # traverse (traverse (fieldDefinitionToIField env))

fieldDefinitionToIField :: Env -> AST.FieldDefinition -> Err ITypes.IField
fieldDefinitionToIField env (AST.FieldDefinition fd) = do
  args <- traverse (inputValueDefinitionToIInputValue env) $ maybe Nil unwrap fd.argumentsDefinition
  tipe <- typeToIType env fd.type
  pure $ ITypes.IField $
    { args
    , deprecationReason: Nothing --  fd.deprecationReason
    , description: fd.description
    , isDeprecated: false -- fd.isDeprecated
    , name: fd.name
    , type: tipe
    }

inputValueDefinitionToIField :: Env -> AST.InputValueDefinition -> Err ITypes.IInputValue
inputValueDefinitionToIField env (AST.InputValueDefinition fd) = do
  tipe <- typeToIType env fd.type
  pure $ ITypes.IInputValue $
    { description: fd.description
    , name: fd.name
    , type: tipe
    , defaultValue: map (unwrap >>> printAst) fd.defaultValue
    }

inputValueDefinitionToIInputValue :: Env -> AST.InputValueDefinition -> Err ITypes.IInputValue
inputValueDefinitionToIInputValue env (AST.InputValueDefinition ivd) = do
  tipe <- typeToIType env ivd.type
  pure $ ITypes.IInputValue
    { defaultValue: map printAst ivd.defaultValue
    , description: ivd.description
    , name: ivd.name
    , type: tipe
    }

typeToIType :: Env -> AST.Type -> Err ITypes.IType
typeToIType env = case _ of
  AST.Type_NamedType named -> namedTypeToIType env named
  AST.Type_ListType l -> listTypeToIType env l
  AST.Type_NonNullType t -> nonNullTypeToIType env t

namedTypeToIType :: Env -> AST.NamedType -> Err ITypes.IType
namedTypeToIType env named = do
  t <- lookupType env named
  fields <- getTypeDefFields t <#> unwrap # traverse (traverse (fieldDefinitionToIField env))
  possibleTypes <- traverse (traverse (namedTypeToIType env)) $ getTypeDefPossibleValues t
  pure $
    ITypes.IType
      { kind: getTypeDefKind t
      , name: Just $ getTypeDefName t
      , description: getTypeDefDescription t
      , fields: \_ -> fields
      , interfaces: Nothing
      , possibleTypes
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Nothing
      }

listTypeToIType :: Env -> AST.ListType -> Err ITypes.IType
listTypeToIType env (AST.ListType t) = do
  ofType <- typeToIType env t
  pure $
    ITypes.IType
      { kind: ITypes.LIST
      , name: Nothing
      , description: Nothing
      , fields: \_ -> Nothing
      , interfaces: Nothing
      , possibleTypes: Nothing
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Just ofType
      }

nonNullTypeToIType :: Env -> AST.NonNullType -> Err ITypes.IType
nonNullTypeToIType env t = do
  ofType <- case t of
    AST.NonNullType_NamedType named -> namedTypeToIType env named
    AST.NonNullType_ListType l -> listTypeToIType env l
  pure $
    ITypes.IType
      { kind: ITypes.NON_NULL
      , name: Nothing
      , description: Nothing
      , fields: \_ -> Nothing
      , interfaces: Nothing
      , possibleTypes: Nothing
      , enumValues: \_ -> Nothing
      , inputFields: Nothing
      , ofType: Just ofType
      }

lookupType :: Env -> AST.NamedType -> Err AST.TypeDefinition
lookupType { namedTypes } (AST.NamedType name) =
  Map.lookup name namedTypes
    # note (pure $ NamedTypeNotDefined name)

-- class QueryType root where
--   queryType :: Env -> root -> Err ITypes.IType

-- instance
--   ( IsSymbol name
--   , GqlTypeName q name
--   ) =>
--   QueryType (GqlRoot query mutation) where
--   queryType env _ = do 

--     pure $ ITypes.IType
--       { kind: ITypes.OBJECT
--       , name: Just $ reflectSymbol (Proxy :: Proxy name)
--       , description: Nothing
--       , fields: \{} -> Nothing
--       , interfaces: Nothing
--       , possibleTypes: Nothing
--       , enumValues: \{} -> Nothing
--       , inputFields: Nothing
--       , ofType: Nothing
--       }

--     where
--     (AST.SchemaDefinition queryDef) = getSchemaDefinition (Proxy :: Proxy name)

todo :: forall a. a
todo = unsafeCrashWith "TODO"