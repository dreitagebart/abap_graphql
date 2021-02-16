import React, { useEffect } from "react"
import { gql } from "@apollo/client"
import { RouteComponentProps } from "react-router-dom"

import { client } from "../utils"

interface Props extends RouteComponentProps {}

export const Apollo: React.FC<Props> = () => {
  useEffect(() => {
    client.mutate({
      mutation: gql`
        mutation UpdateRates($id: String!) {
          updateRates(id: $id) {
            kuhl
          }
        }
      `,
    })

    client
      .query({
        query: gql`
          query GetRates($curreny: String!) {
            rates(currency: $currency) {
              currency
            }
          }
        `,
        variables: { currency: "USD" },
      })
      .then((result) => {
        debugger
        console.log(result)
      })
      .catch((error) => {
        console.error(error)
      })
  }, [])

  return (
    <div>
      <h1>Apollo Test</h1>
    </div>
  )
}
