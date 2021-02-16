import { buildSchema, GraphQLObjectType } from "graphql"
import { SchemaInfo } from "../components"

export const getSchemaInfo = (
  object: string,
  query: string,
  mutation: string
): SchemaInfo => {
  const objects: Array<string> = []
  const queries: Array<string> = []
  const mutations: Array<string> = []

  if (object) {
    buildSchema(`${object} ${query} ${mutation}`, {})
      .toConfig()
      .types.map((type) => {
        if (type.astNode?.kind === "ObjectTypeDefinition") {
          if (type.name === "Query") {
            Object.keys((type as GraphQLObjectType).getFields()).map((key) =>
              queries.push(key)
            )

            return true
          }

          if (type.name === "Mutation") {
            Object.keys((type as GraphQLObjectType).getFields()).map((key) =>
              mutations.push(key)
            )
          }

          return objects.push(type.name)
        }

        return false
      })
  }

  return { objects, queries, mutations }
}
