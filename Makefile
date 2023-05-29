build:
	dune build

run:
	dune build bin/interpreter.exe
	dune exec ./bin/interpreter.exe "./programs/test.txt"

suite:
	dune build test/test.exe
	dune exec ./test/test.exe

repl:
	dune build bin/repl.exe
	dune exec ./bin/repl.exe