all : error_coverage

TOPDIR=..
include $(TOPDIR)/config/Makefile
include $(TOPDIR)/Makefile.tools


PPX_LIBS= $(addprefix $(TOPDIR)/compilerlibs/, ocamlcommon)

PPX_INCLUDES= $(addprefix -I $(TOPDIR)/, parsing utils)

error_coverage: $(PPX_LIBS:=.cma) error_coverage.ml
	$(OCAMLC) $(PPX_INCLUDES) -custom -linkall -o $@ $^
