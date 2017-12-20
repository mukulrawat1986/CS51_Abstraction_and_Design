all: lab1 tests

lab1: lab1.ml
	ocamlbuild lab1.byte

tests: lab1_tests.ml
	ocamlbuild lab1_tests.byte	

clean:
	rm -rf _build *.byte
