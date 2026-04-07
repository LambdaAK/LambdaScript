/// <reference types="vite/client" />

declare module '*.md?raw' {
  const content: string
  export default content
}

/** js_of_ocaml bundle (see browser/forge_browser.ml) */
interface ForgePlaygroundGlobal {
  eval: (code: string) => string
  reset?: () => void
}

interface Window {
  ForgePlayground?: ForgePlaygroundGlobal
}
