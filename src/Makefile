PACKS = ppx_deriving.std
#OCAMLYACC = menhir
#YFLAGS = --fixed-exception
SOURCES = \
  error.ml type.ml sbst.ml location.ml \
  piSyntax.ml parser.mly lexer.mll \
  env.ml constraintsT.ml constraintsR.ml stype.ml \
  constraintsL.ml seqSyntax.ml ttype.ml \
  main.ml
RESULT = ../main

all: byte-code byte-code-library

clean::
	rm -f parser.conflicts

include OCamlMakefile
