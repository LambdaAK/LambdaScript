build:
	dune build

run:
	dune build bin/interpreter.exe
	dune exec ./bin/interpreter.exe "./programs/test.txt"

suite:
	dune build test/test.exe
	dune exec ./test/test.exe

bisect:
	dune build test/test.exe
	dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

repl:
	dune build bin/repl.exe
	dune exec ./bin/repl.exe

old_repl:
	dune build bin/old_repl.exe
	dune exec ./bin/old_repl.exe

doc:
	dune build @doc

opendoc:
	open "_build/default/_doc/_html/index.html"

cloc:
	cloc .

parser_tester:
	dune build bin/parser_tester.exe
	dune exec ./bin/parser_tester.exe