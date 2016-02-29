RESULT = 1w
SOURCES = \
  config.ml \
  httpParser.ml \
  request.ml \
  connection.ml \
  main.ml
PACKS = \
  batteries \
  lwt \
  lwt.unix \
  uri
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
