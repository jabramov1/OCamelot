.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f ocamelot.zip
	zip -r ocamelot.zip . -x@exclude.lst

clean:
	dune clean
	rm -f ocamelot.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
