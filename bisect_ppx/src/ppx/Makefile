ROOTDIR = ../../..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

directories := $(addprefix $(ROOTDIR)/,stdlib otherlibs/unix)

include_directories := $(addprefix -I , $(directories))

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

# The bytecode compiler to use
ocamlc := $(ROOTDIR)/runtime/ocamlrun $(ROOTDIR)/ocamlc $(flags)

# The library we want to build

.PHONY: all

all: bisect_ppx.runtime.cma

bisect_ppx.runtime.cma: version.cmo common.cmo extension.cmo runtime.cmo
	$(ocamlc) -a -o $@ unix.cma $^

# Generic rules

%.cmo: %.ml
	$(ocamlc) -c $<
%.cmi: %.mli
	$(ocamlc) -c $<

%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	ocamlyacc $<

# Dependencies as provided by ocamldep

comments.cmo : \
    comments.cmi
    comments.cmi
comments.cmi :
exclude.cmo : \
    exclude.cmi
    exclude.cmi
exclude.cmi :
exclusions.cmo : \
    exclude.cmi \
    exclusions.cmi
    exclusions.cmi
exclusions.cmi :
instrument.cmo : \
    exclusions.cmi \
    comments.cmi \
    instrument.cmi
    instrument.cmi
instrument.cmi :
register.cmo : \
    instrument.cmi \
    exclusions.cmi \
    comments.cmi
