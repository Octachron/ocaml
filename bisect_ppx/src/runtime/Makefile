ROOTDIR = ../../..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

directories := $(addprefix $(ROOTDIR)/,stdlib bisect)

include_directories := $(addprefix -I , $(directories))

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

# The bytecode compiler to use
ocamlc := $(ROOTDIR)/runtime/ocamlrun $(ROOTDIR)/ocamlc $(flags)

# The library we want to build

.PHONY: all

all: bisect_ppx.runtime.cma

bisect_ppx.runtime.cma: version.cmo common.cmo extension.cmo runtime.cmo bisect.cmo
	$(ocamlc) -a -o $@ $^

# Generic rules

%.cmo: %.ml
	$(ocamlc) -c $<
%.cmi: %.mli
	$(ocamlc) -c $<

# Dependencies as provided by ocamldep

common.cmo : common.cmi
common.cmi :
extension.cmo : extension.cmi
extension.cmi :
runtime.cmo : extension.cmi common.cmi runtime.cmi
runtime.cmi :
version.cmo : version.cmi
version.cmi :
