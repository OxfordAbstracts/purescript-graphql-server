module GraphQL.Resolver.HandleQuery where

import Prelude

import Data.Argonaut (Json)
import GraphQL.Resolver.ToResolver (class ToJsonResolver)
import GraphQL.Resolver.JsonResolver (Resolver)


-- handleQuery :: forall m a . Resolver m -> String -> m Json
-- handleQuery resolver query = do
--   result <- runResolver resolver query
--   case result of
--     Left err -> pure $ encodeJson err
--     Right json -> pure json