/**
 * Copy the js_of_ocaml bundle into public/ for Vite (dev + build).
 * Run from repo: dune build browser/forge_browser.bc.js
 */
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const websiteDir = path.dirname(path.dirname(fileURLToPath(import.meta.url)))
const repoRoot = path.dirname(websiteDir)
const src = path.join(repoRoot, '_build', 'default', 'browser', 'forge_browser.bc.js')
const destDir = path.join(websiteDir, 'public')
const dest = path.join(destDir, 'forge_browser.js')

if (!fs.existsSync(src)) {
  console.warn(
    '[copy-forge-browser] Skip: %s not found. Build with: dune build browser/forge_browser.bc.js',
    src
  )
  process.exit(0)
}

fs.mkdirSync(destDir, { recursive: true })
try {
  fs.unlinkSync(dest)
} catch {
  /* absent */
}
fs.copyFileSync(src, dest)
try {
  fs.chmodSync(dest, 0o644)
} catch {
  /* best effort */
}
console.log('[copy-forge-browser] Wrote', dest)
