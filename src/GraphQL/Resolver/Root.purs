module GraphQL.Resolver.Root where
data GqlRoot q m = GqlRoot { query :: q, mutation :: m }
