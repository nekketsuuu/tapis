PACKS = ppx_deriving.std
#OCAMLYACC = menhir
#YFLAGS = --fixed-exception
SOURCES = \
  error.ml sbst.ml type.ml location.ml \
  piSyntax.ml parser.mly lexer.mll \
  env.ml constraintsT.ml constraintsR.ml stype.ml \
  main.ml
RESULT = main

all: byte-code byte-code-library

clean::
	rm -f parser.conflicts

include OCamlMakefile
