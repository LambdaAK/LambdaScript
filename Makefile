build:
	dune build

# Run a program with the interpreter: make run [FILE=path] (default: programs/minimal.forge).
run:
	dune build bin/interpreter.exe
	dune exec ./bin/interpreter.exe "$(or $(FILE),programs/minimal.forge)"

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

# Optional: FILE=path — preload that program after prelude (same as: dune exec ./bin/repl.exe path).
repl:
	dune build bin/repl.exe
	@if [ -n "$(FILE)" ]; then dune exec ./bin/repl.exe "$(FILE)"; else dune exec ./bin/repl.exe; fi

repl-with-file:
	dune build bin/repl.exe
	@if [ -z "$(FILE)" ]; then echo "Usage: make repl-with-file FILE=path  (or: make repl FILE=path)"; exit 1; fi
	dune exec ./bin/repl.exe "$(FILE)"

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

# Print the parse tree of a file: make parsetest FILE=path/to/file.forge
parsetest:
	@if [ -z "$(FILE)" ]; then echo "Usage: make parsetest FILE=path/to/file.forge"; exit 1; fi
	dune build bin/parse_file.exe
	dune exec ./bin/parse_file.exe "$(FILE)"

# Print Min_ir for a Forge source file (example: make dump-ir FILE=programs/minimal.forge)
dump-ir:
	@if [ -z "$(FILE)" ]; then echo "Usage: make dump-ir FILE=path/to/file.forge"; exit 1; fi
	dune build bin/dump_min_ir.exe
	dune exec ./bin/dump_min_ir.exe "$(FILE)"

# Compile .forge: writes <basename>.mir (Min IR), .ll, .s next to the source, plus the executable (default a.out).
# Example: make compile-forge FILE=programs/minimal.forge OUT=./mybin
compile-forge:
	@if [ -z "$(FILE)" ]; then echo "Usage: make compile-forge FILE=path/to/file.forge [OUT=name]"; exit 1; fi
	dune build bin/compile_forge.exe
	@if [ -n "$(OUT)" ]; then dune exec ./bin/compile_forge.exe "$(FILE)" "$(OUT)"; else dune exec ./bin/compile_forge.exe "$(FILE)"; fi

# The reference manual (documentation/Forge.pdf); needs a LaTeX distribution
# with latexmk. Its examples are checked by test/manual_tests.ml.
docs:
	cd documentation && latexmk -pdf -interaction=nonstopmode -halt-on-error Forge.tex

docs-clean:
	cd documentation && latexmk -C Forge.tex

docs-open:
	open documentation/Forge.pdf