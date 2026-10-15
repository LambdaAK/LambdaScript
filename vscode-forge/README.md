# Forge for VS Code and Cursor

Language support for [Forge](https://github.com/LambdaAK/Forge), a statically typed functional language.

## Features

- **Errors as you type:** lexical, syntax, import and type errors, one per failing definition, plus warnings for `case` expressions that miss a value or have an arm that can never match.
- **Hover:** the inferred type of any name (the type at that use, for polymorphic functions), with its doc comment (the `//` lines above a definition). Works for locals, parameters, pattern variables, constructors, types, traits, methods and built-ins.
- **Go to definition / type definition:** also into imported files and the prelude.
- **Find references, highlight occurrences, rename:** across the workspace. Built-in and prelude names are protected.
- **Completion:** names in scope with their types, module members after `Mod.`, keywords and snippets.
- **Inlay hints:** the inferred type after every top-level definition without an annotation.
- **Outline and breadcrumbs:** modules, types with their constructors, traits with their methods, functions and values. Workspace symbol search (`⌘T`) works too.
- **Highlighting:** a TextMate grammar plus semantic highlighting from the type checker (functions, parameters, constructors, traits, macros, modules).
- **Folding:** definitions, `trait`/`impl`/`mod` blocks and comments.
- **Run:** *Forge: Run File* (`⌘⌥R`, or the ▶ button), *Compile to Native Code and Run*, and *Open REPL*. Code lenses at the top of each file offer the same.

## Requirements

The extension bundles the Forge tools (`forge_ide`, the interpreter, the REPL and the native compiler) with the prelude.

Inside a Forge checkout it uses that checkout's `_build/default/bin` instead, so run `dune build` there. To use a different build, set **forge.toolchainPath**.

Native compilation also needs Clang and the Boehm GC (`brew install bdw-gc` / `apt install libgc-dev`).

## Settings

| Setting | Default | |
|---|---|---|
| `forge.toolchainPath` | empty | Directory with `forge_ide`, `interpreter`, `repl`, `compile_forge` |
| `forge.inlayHints.enabled` | `true` | Show inferred types inline |

## Building the extension

```bash
dune build                      # at the repository root
cd vscode-forge
npm install
npm run package                 # bundles the tools and writes forge-<version>.vsix
cursor --install-extension forge-0.2.0.vsix   # or: code --install-extension …
```
