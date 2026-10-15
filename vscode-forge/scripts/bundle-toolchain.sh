#!/bin/sh
# Copy the Forge tools, the prelude and the C runtime into server/ so that the
# packaged extension works on any .forge file. Run after `dune build` at the
# repository root.
set -e
here=$(cd "$(dirname "$0")/.." && pwd)
root=$(cd "$here/.." && pwd)
bin="$root/_build/default/bin"
for tool in forge_ide interpreter repl compile_forge; do
  if [ ! -x "$bin/$tool.exe" ]; then
    echo "missing $bin/$tool.exe: run 'dune build' in $root first" >&2
    exit 1
  fi
done
rm -rf "$here/server"
mkdir -p "$here/server/bin" "$here/server/prelude" "$here/server/runtime"
for tool in forge_ide interpreter repl compile_forge; do
  cp "$bin/$tool.exe" "$here/server/bin/$tool"
  chmod 755 "$here/server/bin/$tool"
done
cp "$root/prelude/prelude.forge" "$here/server/prelude/"
cp "$root/runtime/ls_runtime.c" "$here/server/runtime/"
echo "bundled the Forge tools into $here/server"
