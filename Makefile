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

repl-with-file:
	dune build bin/repl.exe
	dune exec ./bin/repl.exe "$(FILE)"

repl-test:
	dune build bin/repl.exe
	dune exec ./bin/repl.exe "./programs/test.txt"

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

typecheck:
	dune build bin/typecheck_tester.exe
	dune exec ./bin/typecheck_tester.exe

parsetest:
	dune build bin/parse_file.exe
	dune exec ./bin/parse_file.exe "$(FILE)"

paper:
	cd paper && pdflatex conference_101719.tex
	cd paper && pdflatex conference_101719.tex

paper-clean:
	cd paper && rm -f *.aux *.log *.out *.bbl *.blg *.toc *.lof *.lot

paper-open:
	open paper/conference_101719.pdf