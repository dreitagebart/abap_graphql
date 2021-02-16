import React, { useEffect, useState } from "react"
import { BrowserRouter } from "react-router-dom"
import { api, getSchemaInfo } from "../../utils"

import { Content, Footer, Header } from "../Layout"
import { defaultFiles, Provider, SchemaInfo, Files } from "./Context"
import { SApp } from "./Styled"

interface Props {}

export const App: React.FC<Props> = () => {
  const [files, setFiles] = useState<Files>(defaultFiles)
  const [code, setCode] = useState("")
  const [schema, setSchema] = useState<SchemaInfo>({
    objects: [],
    mutations: [],
    queries: []
  })

  useEffect(() => {
    api.post.loadSchema().then((response) => {
      if (response.data) {
        const { object, query, mutation } = response.data

        setSchema(getSchemaInfo(object, query, mutation))
      }

      setFiles((f) => ({
        ...f,
        object: { ...f.object, value: response.data.object },
        query: { ...f.query, value: response.data.query },
        mutation: { ...f.mutation, value: response.data.mutation }
      }))
      setCode(response.data.data)
    })
  }, [])

  return (
    <Provider
      value={{
        files,
        setFiles,
        code,
        setCode,
        schema,
        setSchema,
        connection: process.env.REACT_APP_SAP_CONNECTION!
      }}
    >
      <BrowserRouter>
        <SApp></SApp>
        <Header></Header>
        <Content></Content>
        <Footer></Footer>
      </BrowserRouter>
    </Provider>
  )
}

export default App
