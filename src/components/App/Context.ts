import { createContext, Dispatch, SetStateAction } from "react"

export type FileNames = "object" | "query" | "mutation"

export type Files = {
  [name: string]: {
    name: string
    language: string
    value: string
  }
}

export interface SchemaInfo {
  objects: Array<string>
  queries: Array<string>
  mutations: Array<string>
}

interface AppContext {
  files: Files
  setFiles: Dispatch<SetStateAction<Files>>
  connection: string
  code: string
  schema: SchemaInfo
  setSchema: Dispatch<SetStateAction<SchemaInfo>>
  setCode: Dispatch<SetStateAction<string>>
}

export const defaultFiles: Files = {
  object: {
    name: "object.graphql",
    language: "graphql",
    value: ""
  },
  query: {
    name: "query.graphql",
    language: "graphql",
    value: ""
  },
  mutation: {
    name: "mutation.graphql",
    language: "graphql",
    value: ""
  }
}

export const Context = createContext<AppContext>({
  files: defaultFiles,
  setFiles: () => null,
  code: "",
  schema: { objects: [], queries: [], mutations: [] },
  setSchema: () => null,
  setCode: () => null,
  connection: ""
})

export const { Provider } = Context
