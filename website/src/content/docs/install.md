# Install & tooling

Forge lives in this repository. Build the OCaml project with **Dune**; binaries are produced under `_build/default/bin/`.

## Build

From the repository root:

```bash
dune build
```

This builds the interpreter, REPL, compiler (`compile_forge`), and other tools.

## REPL

```bash
dune exec ./bin/repl.exe
```

Optional preload file:

```bash
dune exec ./bin/repl.exe programs/minimal.ls
```

## Website & Playground

The site is a **Vite + React** app under `website/`.

### In-browser evaluator (js_of_ocaml) — recommended for static hosting

The Playground can run **entirely in the visitor’s browser** (no Node runner). Build the JS bundle and copy it into `website/public/`:

**Dependencies (once):**

```bash
opam install js_of_ocaml-compiler js_of_ocaml
```

**Each time you change the evaluator or prelude:**

```bash
dune build browser/forge_browser.bc.js
npm run sync:forge-js --prefix website
```

`npm run dev` / `npm run build` run `sync:forge-js` automatically via `predev` / `prebuild` if the `.bc.js` file exists. The copied file `public/forge_browser.js` is gitignored (~4 MiB).

### Optional: local Node playground API

For development without rebuilding the JS bundle, a small **Node** server can drive the native `playground` binary instead:

1. `dune build bin/playground.exe`
2. `npm run server --prefix website` (listens on `127.0.0.1:8787` by default)
3. `npm run dev --prefix website` — Vite proxies `/api/*` to that server

If `forge_browser.js` is present, the UI prefers the **in-browser** engine and does not need the API.

## Production

- **Static hosting only:** build `forge_browser.bc.js`, run `npm run build` in `website/` (with `prebuild` copying the bundle), deploy `dist/`. Visitors load the evaluator as a single script; no server-side execution.
- **API mode:** ship the `playground` binary and the Node adapter behind your reverse proxy only if you intentionally avoid the browser bundle.
