import React, { useContext, useState } from "react"
import { SideNav } from "fundamental-react"
import { Link, NavLink } from "react-router-dom"

import { SSidebar } from "./Styled"
import { Context } from "../App"
import { ImportDialog } from "../Dialog"

interface Props {}

export const Sidebar: React.FC<Props> = () => {
  const { schema } = useContext(Context)
  const [dialog, setDialog] = useState(false)
  const [condensed, setCondensed] = useState(false)

  return (
    <SSidebar>
      <ImportDialog
        show={dialog}
        onClose={() => setDialog(false)}
      ></ImportDialog>
      <SideNav condensed={condensed} skipLink={{ href: "/", label: "Skip" }}>
        <SideNav.List>
          <SideNav.ListItem glyph="tree" url="#" name="Schema" expanded>
            <SideNav.List compact>
              <SideNav.ListItem
                glyph="database"
                url="#"
                name="Object"
                isSubItem
              >
                <SideNav.List compact level={2}>
                  {schema.objects.map((name) => {
                    return (
                      <SideNav.ListItem key={name} glyph="rhombus-milestone">
                        <NavLink
                          to={`/editor/object/${name}`}
                          activeStyle={{ fontWeight: "bold" }}
                        >
                          {name}
                        </NavLink>
                      </SideNav.ListItem>
                    )
                  })}
                </SideNav.List>
              </SideNav.ListItem>
              <SideNav.ListItem glyph="trip-report" url="#" name="Query">
                <SideNav.List compact level={2}>
                  {schema.queries.map((name) => {
                    return (
                      <SideNav.ListItem key={name} glyph="rhombus-milestone">
                        <NavLink
                          to="/editor/query/getMaterial"
                          activeStyle={{ fontWeight: "bold" }}
                        >
                          {name}
                        </NavLink>
                      </SideNav.ListItem>
                    )
                  })}
                </SideNav.List>
              </SideNav.ListItem>
              <SideNav.ListItem glyph="syringe" url="#" name="Mutation">
                <SideNav.List compact level={2}>
                  {schema.mutations.map((name) => {
                    return (
                      <SideNav.ListItem key={name} glyph="rhombus-milestone">
                        <NavLink
                          to="/editor/query/getMaterial"
                          activeStyle={{ fontWeight: "bold" }}
                        >
                          {name}
                        </NavLink>
                      </SideNav.ListItem>
                    )
                  })}
                </SideNav.List>
              </SideNav.ListItem>
            </SideNav.List>
          </SideNav.ListItem>
          <SideNav.ListItem glyph="source-code">
            <Link to="/editor">Editor</Link>
          </SideNav.ListItem>
          <SideNav.ListItem glyph="lab">
            <Link to="/test">Test</Link>
          </SideNav.ListItem>
          <SideNav.ListItem glyph="lab">
            <Link to="/apollo">Apollo</Link>
          </SideNav.ListItem>
          <SideNav.ListItem
            glyph="add-activity"
            onClick={() => setDialog(true)}
          >
            <span style={{ cursor: "pointer" }}>Import</span>
          </SideNav.ListItem>
        </SideNav.List>
        <SideNav.List isUtility>
          <SideNav.ListItem
            glyph={condensed ? "open-command-field" : "close-command-field"}
            id="item-3"
            onClick={() => {
              setCondensed(!condensed)
            }}
          >
            <span style={{ cursor: "pointer" }}></span>
          </SideNav.ListItem>
        </SideNav.List>
      </SideNav>
    </SSidebar>
  )
}
