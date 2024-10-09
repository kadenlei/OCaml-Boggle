.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe
play:
	OCAMLRUNPARAM=b dune exec bin/boggle.exe
check:
	@bash check.sh

clean:
	dune clean
docs:
	dune build @doc