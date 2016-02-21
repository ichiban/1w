RESULT = 1w
SOURCES = \
  configuration.ml \
  httpParser.ml \
  connection.ml \
  main.ml
PACKS = \
  batteries \
  lwt \
  lwt.unix
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)