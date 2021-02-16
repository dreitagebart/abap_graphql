import React from "react"
import { RouteComponentProps } from "react-router-dom"

import { FileNames, GraphQLEditor } from "../components"

interface Props
  extends RouteComponentProps<{ type: FileNames; name: string }> {}

export const Editor: React.FC<Props> = ({
  match: {
    params: { type, name }
  }
}) => {
  return <GraphQLEditor type={type} name={name}></GraphQLEditor>
}
