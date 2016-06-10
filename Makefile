RESULT = 1w
SOURCES = \
  config.ml \
  httpParser.ml \
  request.ml \
  response.ml \
  handler.ml \
  connection.ml \
  server.ml \
  main.ml
PACKS = \
  batteries \
  lwt \
  lwt.unix \
  lwt.ppx \
  uri
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
