# Forge overview

Forge is a **statically typed functional language** with Hindley–Milner style inference, algebraic data types, pattern matching, and (via the standard prelude) traits (typeclasses). This repository implements an **interpreter** and an **ahead-of-time compiler** to native code (LLVM + Clang).

## Toolchain

- **Interpreter** — evaluate `.ls` programs.
- **REPL** — type and run snippets with persistent bindings; prelude loaded once at startup.
- **Compiler** — `compile_forge` lowers programs through Min IR to LLVM IR and assembly.

## Standard prelude

Most programs are used together with `prelude/prelude.ls` in the repository: it defines `List`, `Option`, `Ordering`, the trait hierarchy (`Functor`, `Monad`, …), and common instances. The **Playground** on this site uses the same prelude as the REPL.

## Learn more

Use the sidebar for **types**, **expressions**, **pattern matching**, **traits**, **built-ins**, and **prelude** reference pages. Try code in the **Playground** (requires the local playground server — see Install).
