import React, {
  Fragment,
  useCallback,
  useContext,
  useRef,
  useState,
  MouseEvent,
  useEffect
} from "react"
import { ActionBar, Button, TabGroup, Tab } from "fundamental-react"
import CodeEditor, { Monaco } from "@monaco-editor/react"
import { editor } from "monaco-editor"

import { Context, FileNames } from "../App"
import { api, getSchemaInfo } from "../../utils"

interface Props {
  type: FileNames
  name: string
}

export const GraphQLEditor: React.FC<Props> = ({ type = "object", name }) => {
  const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null)
  const monacoRef = useRef<Monaco | null>(null)
  const [fileName, setFileName] = useState<FileNames>(type)
  const { setSchema, files, setFiles } = useContext(Context)

  const handleValidation = useCallback((markers: Array<editor.IMarker>) => {
    markers.forEach((marker) => console.log("onValidate:", marker.message))
  }, [])

  const handleSave = useCallback(
    (event: MouseEvent<HTMLButtonElement>) => {
      event.preventDefault()

      setSchema(
        getSchemaInfo(
          files.object.value,
          files.query.value,
          files.mutation.value
        )
      )

      api.post.saveSchema({
        object: files.object.value,
        query: files.query.value,
        mutation: files.mutation.value
      })
    },
    [files, setSchema]
  )

  const handleCodeChange = useCallback(
    (value) => {
      if (value) {
        setFiles((f) => ({ ...f, [fileName]: { ...f[fileName], value } }))
      }
    },
    [setFiles, fileName]
  )

  const handleEditorMount = (
    editor: editor.IStandaloneCodeEditor,
    monaco: Monaco
  ) => {
    monacoRef.current = monaco
    editorRef.current = editor
  }

  useEffect(() => {
    setFileName(type)
  }, [type])

  // useEffect(() => {
  //   if (monaco && type && name) {
  //     let search = ""

  //     switch (type) {
  //       case "object": {
  //         search = `type ${name}`
  //         break
  //       }
  //       case "query": {
  //         search = `${name}`
  //         break
  //       }
  //       case "mutation": {
  //         search = `${name}`
  //         break
  //       }
  //       default: {
  //         break
  //       }
  //     }

  //     monaco.editor.getModels().map((model) => {
  //       const matches = model.findMatches(
  //         search,
  //         true,
  //         false,
  //         true,
  //         null,
  //         true,
  //         1
  //       )

  //       if (matches.length === 1) {
  //         const range = matches[0].range

  //         editorRef.current?.revealRangeAtTop(range, 0)
  //       }

  //       return false
  //     })
  //   }
  // }, [monaco, type, name, editorRef])

  return (
    <Fragment>
      <ActionBar
        actions={
          <Button option="emphasized" onClick={handleSave} glyph="save">
            Save
          </Button>
        }
        description={"Action Bar Description"}
        title="GraphQL Editor"
      />
      <TabGroup
        selectedIndex={Object.keys(files).indexOf(fileName)}
        onTabClick={(event, index) => {
          Object.keys(files).map((key, i) => {
            if (index === i) {
              setFileName(key as FileNames)
            }
            return false
          })
        }}
      >
        <Tab title="Object" id="object">
          <CodeEditor
            height="90vh"
            width="100%"
            theme="vs-dark"
            path={files.object.name}
            defaultLanguage={files.object.language}
            defaultValue={files.object.value}
            onMount={handleEditorMount}
            onChange={handleCodeChange}
            onValidate={handleValidation}
          ></CodeEditor>
        </Tab>
        <Tab
          title="Query"
          id="query"
          onClick={() => {
            debugger
            setFileName("query")
          }}
        >
          <CodeEditor
            height="90vh"
            width="100%"
            theme="vs-dark"
            path={files.query.name}
            defaultLanguage={files.query.language}
            defaultValue={files.query.value}
            onMount={handleEditorMount}
            onChange={handleCodeChange}
            onValidate={handleValidation}
          ></CodeEditor>
        </Tab>
        <Tab
          title="Mutation"
          id="mutation"
          onClick={() => {
            debugger
            setFileName("mutation")
          }}
        >
          <CodeEditor
            height="90vh"
            width="100%"
            theme="vs-dark"
            path={files.mutation.name}
            defaultLanguage={files.mutation.language}
            defaultValue={files.mutation.value}
            onMount={handleEditorMount}
            onChange={handleCodeChange}
            onValidate={handleValidation}
          ></CodeEditor>
        </Tab>
      </TabGroup>
    </Fragment>
  )
}
