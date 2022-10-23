module Test.GraphQL.Server.Schema.Introspection.GetType (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), find, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect.Exception (Error)
import GraphQL.GqlRep (class GqlRep, GObject)
import GraphQL.Resolver.ToResolver (class ToResolver, objectResolver)
import GraphQL.Server.Schema.Introspection.GetType (class GetGqlType, genericGetGqlType, getITypeWithNullable)
import GraphQL.Server.Schema.Introspection.Types (IField(..), IInputValue(..), IType(..), ITypeKind(..), IType_T, defaultIField, defaultIType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "Server.Schema.Introspection.GetType" do
    describe "getITypeWithNullable" do

      it "should return the gql type of scalars" do
        (Proxy :: Proxy Int) `shouldBeGqlType` notNull _ { name = Just "Int" }
        (Proxy :: Proxy Number) `shouldBeGqlType` notNull _ { name = Just "Float" }

      it "should return the gql type of compound types" do
        (Proxy :: Proxy (Array String)) `shouldBeGqlType`
          notNull _
            { kind = LIST
            , ofType = Just $ IType $ notNull _ { name = Just "String" }
            }
        (Proxy :: Proxy (Array String)) `shouldBeGqlType`
          notNull _
            { kind = LIST
            , ofType = Just $ IType $ notNull _ { name = Just "String" }
            }
        (Proxy :: Proxy (Maybe String)) `shouldBeGqlType` defaultIType { name = Just "String" }

      it "should return the gql type of a single property record" do
        (Proxy :: Proxy (Maybe T1)) `shouldBeGqlType`
          defaultIType
            { kind = OBJECT
            , name = Just "T1"
            , fields = \_ -> Just $
                IField defaultIField
                  { name = "query"
                  , type = IType $ defaultIType { name = Just "String" }
                  }
                  : Nil
            }

      it "should return the gql type of a recursive record" do
        let
          testObjField expectedName t =
            case find (unwrap >>> _.name >>> eq "query") =<< t.fields { includeDeprecated: Nothing } of
              Just (IField f) -> do
                f.name `shouldEqual` "query"
                case f.type of
                  IType t' -> do
                    t'.kind `shouldEqual` OBJECT
                    t'.name `shouldEqual` Just expectedName
                    pure t'
              Nothing -> do
                fail "query field not found"
                pure defaultIType

          (IType t1) = getITypeWithNullable (Proxy :: Proxy (Maybe TRec1))

        t1.kind `shouldEqual` OBJECT
        t1.name `shouldEqual` Just "TRec1"

        t2 <- testObjField "TRec2" t1

        void $ testObjField "TRec1" t2

data T1 = T1 { query :: Maybe String }

derive instance Generic T1 _

instance GqlRep T1 GObject "T1"

instance Applicative m => ToResolver T1 m where
  toResolver a = objectResolver a

instance GetGqlType T1 where
  getType a = genericGetGqlType a

newtype TRec1 = TRec1 { query :: Maybe TRec2 }

derive instance Generic TRec1 _

instance GqlRep TRec1 GObject "TRec1"

instance Applicative m => ToResolver TRec1 m where
  toResolver a = objectResolver a

instance GetGqlType TRec1 where
  getType a = genericGetGqlType a

newtype TRec2 = TRec2 { query :: Maybe TRec1 }

derive instance Generic TRec2 _

instance GqlRep TRec2 GObject "TRec2"

instance Applicative m => ToResolver TRec2 m where
  toResolver a = objectResolver a

instance GetGqlType TRec2 where
  getType a = genericGetGqlType a

notNull :: (IType_T -> IType_T) -> IType_T
notNull fn = defaultIType
  { kind = NON_NULL
  , ofType = Just $ IType $ fn defaultIType
  }

shouldBeGqlType
  :: forall m a
   . MonadThrow Error m
  => GetGqlType a
  => Proxy a
  -> IType_T
  -> m Unit
shouldBeGqlType proxy itype = do
  eqOn_ \{ name } -> { name }
  eqOn_ \{ kind } -> { kind }
  displayIType (getITypeWithNullable proxy) `shouldEqual` displayIType (IType itype)
  where
  (IType result) = getITypeWithNullable proxy

  eqOn_ :: forall p. Show p => Eq p => (IType_T -> p) -> m Unit
  eqOn_ = eqOn result itype

eqOn :: forall a m b. MonadThrow Error m => Show b => Eq b => a -> a -> (a -> b) -> m Unit
eqOn l r f = f l `shouldEqual` f r

displayIType :: IType -> String
displayIType (IType t) = show
  { kind: t.kind
  , name: t.name
  , description: t.description
  , fields: ((map displayIField <$> t.fields { includeDeprecated: Nothing }) :: _ (_ String))
  , interfaces: map displayIType <$> t.interfaces
  , possibleTypes: map displayIType <$> t.possibleTypes
  , enumValues: t.enumValues { includeDeprecated: Nothing }
  , ofType: maybe "" displayIType t.ofType
  }

displayIInputValue :: IInputValue -> String
displayIInputValue (IInputValue f) = show
  { name: f.name -- String
  , description: f.description -- Maybe String
  , type: displayIType f.type -- IType
  , defaultValue: f.defaultValue -- Maybe String
  }

displayIField :: IField -> String
displayIField (IField f) = show
  { name: f.name
  , description: f.description
  , args: map displayIInputValue f.args
  , type: displayIType f.type
  , isDeprecated: f.isDeprecated
  , deprecationReason: f.deprecationReason
  }
