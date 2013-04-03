CAMLP4=
PACK=losic
OCAMLC_OPTS=
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=string_ext.ml option.ml \
	out_message.ml out_parser.ml out_builder.ml \
	ctl_message.ml ctl_builder.ml ctl_parser.ml \
	loop.ml
NON_LIB_MODULES=

BYTE_TARGETS=losic.cma
NATIVE_TARGETS=losic.cmxa

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
