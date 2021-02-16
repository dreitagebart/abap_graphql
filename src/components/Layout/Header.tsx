import React from "react"
import { useHistory } from "react-router-dom"
import { Shellbar } from "fundamental-react"

import { SHeader } from "./Styled"

interface Props {}

export const Header: React.FC<Props> = () => {
  const history = useHistory()

  return (
    <SHeader>
      <Shellbar
        logoSAP
        actions={[
          {
            notificationCount: 0,
            glyph: "action-settings",
            callback: () => {
              history.push("/settings")
            },
          },
        ]}
        // size="xl"
        productTitle="ABAP GraphQL"
      ></Shellbar>
    </SHeader>
  )
}
