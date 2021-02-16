import React, { useCallback, useState } from "react"
import debounce from "lodash.debounce"
import { Button, Dialog, SearchInput, Select } from "fundamental-react"
import CodeEditor from "@monaco-editor/react"

import { api } from "../../utils"
import { SImportHeader } from "./Styled"

interface Props {
  show: boolean
  onClose: () => void
}

export const ImportDialog: React.FC<Props> = ({ show, onClose }) => {
  const [result, setResult] = useState<Array<{ text: string }>>([])
  const [form, setForm] = useState({
    selected: "class",
    object: "",
    sdl: "",
    options: [
      { key: "class", text: "Class" },
      { key: "function", text: "Function module" }
    ]
  })

  const handleImport = useCallback(() => {
    onClose()
  }, [onClose])

  const handleSearch = debounce(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      switch (form.selected) {
        case "class": {
          return api.post
            .searchClass({ search: event.target.value })
            .then((response) => {
              setResult(
                response.data.objects.map(({ key, text }: any) => ({
                  text: `${key}`
                }))
              )
            })
        }
        case "function": {
          return api.post
            .searchFunction({ search: event.target.value })
            .then((response) => {
              setResult(
                response.data.objects.map(({ key, text }: any) => ({
                  text: `${key}`
                }))
              )
            })
        }
      }
    },
    500
  )

  const handleChange = useCallback((value) => {
    if (value) {
      setForm((f) => ({ ...f, sdl: value }))
    }
  }, [])

  const handleGenerate = useCallback(() => {
    switch (form.selected) {
      case "class": {
        return api.post
          .importClass({ object: form.object })
          .then((response) => {
            debugger
          })
      }
      case "function": {
        return api.post
          .importFunction({ object: form.object })
          .then((response) => {
            debugger
          })
      }
    }
  }, [form.object, form.selected])

  return (
    <Dialog
      show={show}
      onClose={onClose}
      title="Import ABAP Object"
      actions={[
        <Button onClick={() => handleImport()} option="emphasized">
          Import
        </Button>,
        <Button
          onClick={() => {
            onClose()
          }}
        >
          Cancel
        </Button>
      ]}
    >
      <SImportHeader>
        <div style={{ marginRight: 8 }}>
          <Select
            selectedKey={form.selected}
            options={form.options}
            onSelect={(event, selected) =>
              setForm((f) => ({ ...f, selected: selected.key }))
            }
          ></Select>
        </div>
        <div style={{ marginRight: 8 }}>
          <SearchInput
            subStringsearch={false}
            placeholder="Development object"
            searchList={result}
            onChange={handleSearch}
          ></SearchInput>
        </div>
        <Button option="emphasized" onClick={handleGenerate}>
          Generate SDL
        </Button>
      </SImportHeader>
      <CodeEditor
        height="90vh"
        width="100%"
        theme="vs-dark"
        language="graphql"
        value={form.sdl}
        onChange={handleChange}
      ></CodeEditor>
    </Dialog>
  )
}
