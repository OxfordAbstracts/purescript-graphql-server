module GraphQL.Server.GqlError where


import Parsing (ParseError)


data GqlError
  = CouldNotParseRequest ParseError
  | NoOperationDefinition
