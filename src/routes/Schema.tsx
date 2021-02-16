import React from "react"
import { RouteComponentProps } from "react-router-dom"
import CodeEditor from "@monaco-editor/react"

interface Props extends RouteComponentProps {}

export const Schema: React.FC<Props> = () => {
  return (
    <div>
      <h1>Schema</h1>
      <CodeEditor
        height="90vh"
        width="100%"
        theme="vs-dark"
        defaultLanguage="typescript"
        defaultValue="// some comment"
      ></CodeEditor>
    </div>
  )
}
