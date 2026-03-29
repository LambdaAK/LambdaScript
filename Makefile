build:
	dune build

run:
	dune build bin/interpreter.exe
	dune exec ./bin/interpreter.exe "./programs/test.txt"

suite:
	dune build test/test.exe
	dune exec ./test/test.exe

# [dune runtest] prints nothing when the runtest alias is already up to date.
# [--force] re-executes tests so OUnit output (dots + summary) is visible.
compiler-suite:
	dune runtest test --force

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

# Print Min_ir for a LambdaScript file (example: make dump-ir FILE=programs/minimal.ls)
dump-ir:
	@if [ -z "$(FILE)" ]; then echo "Usage: make dump-ir FILE=path/to/file.ls"; exit 1; fi
	dune build bin/dump_min_ir.exe
	dune exec ./bin/dump_min_ir.exe "$(FILE)"

# Compile .ls: writes <basename>.mir (Min IR), .ll, .s next to the source, plus the executable (default a.out).
# Example: make compile-ls FILE=programs/minimal.ls OUT=./mybin
compile-ls:
	@if [ -z "$(FILE)" ]; then echo "Usage: make compile-ls FILE=path/to/file.ls [OUT=name]"; exit 1; fi
	dune build bin/compile_lambdascript.exe
	@if [ -n "$(OUT)" ]; then dune exec ./bin/compile_lambdascript.exe "$(FILE)" "$(OUT)"; else dune exec ./bin/compile_lambdascript.exe "$(FILE)"; fi

paper:
	cd paper && pdflatex conference_101719.tex
	cd paper && pdflatex conference_101719.tex

paper-clean:
	cd paper && rm -f *.aux *.log *.out *.bbl *.blg *.toc *.lof *.lot

paper-open:
	open paper/conference_101719.pdf