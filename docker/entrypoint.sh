#!/usr/bin/env sh
set -eu

print_help() {
  cat <<'EOF'
Forge Docker image commands:
  help
  repl [file]
  run <file>
  compile <file>
  shell

Examples:
  forge run /opt/forge/programs/minimal.forge
  forge repl
  forge compile /work/hello.forge   # writes /work/a.out
  forge /work/a.out
EOF
}

cmd="${1:-help}"

case "$cmd" in
  help|-h|--help)
    print_help
    ;;

  repl)
    shift
    exec /usr/local/bin/forge-repl "$@"
    ;;

  run)
    if [ "$#" -ne 2 ]; then
      echo "Usage: forge run <file>" >&2
      exit 1
    fi
    shift
    exec /usr/local/bin/forge-interpreter "$@"
    ;;

  compile)
    if [ "$#" -ne 2 ]; then
      echo "Usage: forge compile <file>" >&2
      exit 1
    fi
    shift
    exec /usr/local/bin/forge-compile "$1" /work/a.out
    ;;

  shell)
    shift
    exec /bin/sh "$@"
    ;;

  *)
    exec "$@"
    ;;
esac
