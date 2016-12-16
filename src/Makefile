PACKS = ppx_deriving.std
OCAMLYACC = menhir --fixed-exception
SOURCES = \
  type.ml location.ml syntax.ml parser.mly lexer.mll \
  main.ml
RESULT = main

all: byte-code byte-code-library

clean::
	rm -f parser.conflicts

include OCamlMakefile
