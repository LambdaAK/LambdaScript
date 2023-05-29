build:
	dune build main.exe

run:
	dune build bin/interpreter.exe
	dune exec ./bin/interpreter.exe "./programs/test.txt"

test:
	dune build test/test.exe
	dune exec test/text.exe

repl:
	dune build bin/repl.exe
	dune exec ./bin/repl.exe