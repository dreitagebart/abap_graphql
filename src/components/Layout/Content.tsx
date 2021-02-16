import React from "react"
import { Switch, Route } from "react-router-dom"

import { SContent, SContentWrapper } from "./Styled"
import { Home, Editor, Map, Settings, Schema, Test, Apollo } from "../../routes"
import { Sidebar } from "./Sidebar"

interface Props {}

export const Content: React.FC<Props> = () => {
  return (
    <SContentWrapper>
      <Sidebar></Sidebar>
      <SContent>
        <Switch>
          <Route exact path="/" component={Home}></Route>
          <Route exact path="/editor" component={Editor}></Route>
          <Route path="/editor/:type/:name" component={Editor}></Route>
          <Route exact path="/test" component={Test}></Route>
          <Route exact path="/map" component={Map}></Route>
          <Route exact path="/apollo" component={Apollo}></Route>
          <Route exact path="/settings" component={Settings}></Route>
          <Route exact path="/schema" component={Schema}></Route>
        </Switch>
      </SContent>
    </SContentWrapper>
  )
}
