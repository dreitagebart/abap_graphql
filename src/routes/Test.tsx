import React, { useContext } from "react"
import GraphiQL from "graphiql"
import { RouteComponentProps } from "react-router-dom"
import "graphiql/graphiql.min.css"

import { Context } from "../components"

interface Props extends RouteComponentProps {}

export const Test: React.FC<Props> = () => {
  const { connection } = useContext(Context)

  return (
    <GraphiQL
      editorTheme="vs-dark"
      fetcher={async (graphQLParams) => {
        debugger
        const data = await fetch(connection, {
          method: "POST",
          headers: {
            Accept: "application/json",
            "Content-Type": "application/json"
          },
          body: JSON.stringify(graphQLParams),
          credentials: "same-origin"
        })
        return data.json().catch(() => data.text())
      }}
    ></GraphiQL>
  )
}
