ROOTDIR = ../../..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

directories := $(addprefix $(ROOTDIR)/,stdlib otherlibs/unix otherlibs/str \
                                        compilerlibs utils typing parsing \
                                        bisect_ppx/src/runtime)

include_directories := $(addprefix -I , $(directories))

#include_cmos := $(shell find $(directories) -name '*.cmo')

#print-%  : ; @echo $* = $($*)

include_cmos := build_path_prefix_map.cmo config.cmo misc.cmo \
        identifiable.cmo numbers.cmo str.cmo syntaxerr.cmo terminfo.cmo warnings.cmo \
        arg_helper.cmo clflags.cmo longident.cmo \
        location.cmo load_path.cmo \
        docstrings.cmo common.cmo \
        ast_helper.cmo ast_mapper.cmo

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

# The bytecode compiler to use
ocamlc := $(ROOTDIR)/runtime/ocamlrun $(ROOTDIR)/ocamlc $(flags)

# The library we want to build

.PHONY: all

all: bisect_ppx

bisect_ppx: comments_lexer.cmo comments.cmo exclude.cmo exclude_parser.cmo exclude_lexer.cmo exclusions.cmo instrument.cmo register.cmo
	$(ocamlc) -o $@ unix.cma $(include_cmos) $^

# Generic rules

%.cmo: %.ml
	$(ocamlc) -c $<
%.cmi: %.mli
	$(ocamlc) -c $<

%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	ocamlyacc $<
%.ml: %.mlp
	mv $< $@
	ocamlfind ppx_tools_versioned/metaquot_409/ppx.exe $@ &> $< | echo "process instrument.mlp"
	sed 's/Ast_409.//g;s/Migrate_parsetree__Ast_409.//g;s/Migrate_parsetree__//g' $< &> $@
	rm $<

# Dependencies as provided by ocamldep

comments.cmo : \
    comments_lexer.cmo \
    comments.cmi
comments.cmx : \
    comments_lexer.cmx \
    comments.cmi
comments.cmi :
comments_lexer.cmo :
comments_lexer.cmx :
exclude.cmo : \
    exclude.cmi
exclude.cmx : \
    exclude.cmi
exclude.cmi :
exclude_lexer.cmo : \
    exclude_parser.cmi \
    exclude.cmi
exclude_lexer.cmx : \
    exclude_parser.cmx \
    exclude.cmx
exclude_parser.cmo : \
    exclude.cmi \
    exclude_parser.cmi
exclude_parser.cmx : \
    exclude.cmx \
    exclude_parser.cmi
exclude_parser.cmi : \
    exclude.cmi
exclusions.cmo : \
    exclude_parser.cmi \
    exclude_lexer.cmo \
    exclude.cmi \
    exclusions.cmi
exclusions.cmx : \
    exclude_parser.cmx \
    exclude_lexer.cmx \
    exclude.cmx \
    exclusions.cmi
exclusions.cmi :
instrument.cmo : \
    exclusions.cmi \
    comments.cmi \
    instrument.cmi
instrument.cmx : \
    exclusions.cmx \
    comments.cmx \
    instrument.cmi
instrument.cmi :
register.cmo : \
    instrument.cmi \
    exclusions.cmi \
    comments.cmi
register.cmx : \
    instrument.cmx \
    exclusions.cmx \
    comments.cmx
