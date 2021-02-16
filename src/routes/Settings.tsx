import React, { useContext } from "react"
import { RouteComponentProps } from "react-router-dom"
import { FormInput, FormItem, FormLabel } from "fundamental-react"

import { Context } from "../components"

interface Props extends RouteComponentProps {}

export const Settings: React.FC<Props> = () => {
  const { connection } = useContext(Context)

  return (
    <div>
      <h1>Settings</h1>
      <FormItem>
        <FormLabel>SAP Connection URL</FormLabel>
        <FormInput readOnly value={connection}></FormInput>
      </FormItem>
    </div>
  )
}
