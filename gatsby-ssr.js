import React from "react"
import { ThemeProvider } from "./src/Mui"

export const wrapRootElement = ({ element }) => {
  return <ThemeProvider.make>{element}</ThemeProvider.make>
}
