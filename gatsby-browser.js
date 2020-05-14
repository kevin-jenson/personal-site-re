import React from "react"
import { ThemeProvider } from "./src/ThemeProvider"

export const wrapRootElement = ({ element }) => {
  return <ThemeProvider.make>{element}</ThemeProvider.make>
}
