ROOTDIR = ..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

directories := $(addprefix $(ROOTDIR)/, stdlib compilerlibs)

include_directories := $(addprefix -I , $(directories))

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-40-41-42-44-45-48 -warn-error A

ocamlc := $(ROOTDIR)/runtime/ocamlrun $(ROOTDIR)/ocamlc $(flags) -custom

libraries := ocamlbytecomp.cma  ocamlcommon.cma

.PHONY: build
build: bisect_ppx

bisect_ppx:
	$(ocamlc) -o $@ $(libraries) $(wildcard src/*.ml) $(wildcard src/*.mli)