CAMLP4=
OCAMLC_OPTS=-package core,async,async_unix,losic
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=fifo_tail.ml irc_msg.ml irc.ml
NON_LIB_MODULES=osic.ml

BYTE_TARGETS=osic.byte
NATIVE_TARGETS=osic.native

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
