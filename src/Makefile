#OCAMLYACC = menhir
#YFLAGS = --fixed-exception
LIBS = unix
SOURCES = \
  error.ml type.ml sbst.ml location.ml \
  piSyntax.ml parser.mly lexer.mll \
  env.ml constraintsT.ml constraintsR.ml stype.ml \
  constraintsL.ml seqSyntax.ml ttype.ml \
  main.ml
# TODO(nekketsuuu): Windows対応
RESULT = ../piterm

all: byte-code byte-code-library

# TODO(nekketsuuu): Windows対応
clean::
	rm -f parser.conflicts

include OCamlMakefile
