import axios, { AxiosResponse } from "axios"

const client = () => {
  const defaultOptions = {
    baseURL: process.env.REACT_APP_SAP_CONNECTION,
    headers: {
      "Content-Type": "application/json"
    }
  }

  const instance = axios.create(defaultOptions)

  instance.interceptors.request.use((config) => {
    return config
  })

  return instance
}

const axiosClient = client()

export const api = {
  post: {
    importClass: (payload: { object: string }): Promise<AxiosResponse<any>> => {
      return axiosClient.post("/importClass", payload)
    },
    importFunction: (payload: {
      object: string
    }): Promise<AxiosResponse<any>> => {
      return axiosClient.post("/importFunction", payload)
    },
    searchFunction: (payload: {
      search: string
    }): Promise<AxiosResponse<any>> => {
      return axiosClient.post("/searchFunction", payload)
    },
    searchClass: (payload: { search: string }): Promise<AxiosResponse<any>> => {
      return axiosClient.post("/searchClass", payload)
    },
    saveSchema: (payload: {
      object: string
      query: string
      mutation: string
    }): Promise<AxiosResponse<any>> => {
      return axiosClient.post("/saveSchema", {
        ...payload,
        operation: "saveSchema"
      })
    },
    loadSchema: (): Promise<AxiosResponse<any>> => {
      return axiosClient.post("/loadSchema", { operation: "loadSchema" })
    }
  },
  get: {}
}
