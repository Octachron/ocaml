#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*             Florian Angeletti, projet Cambium, Inria Paris             *
#*                                                                        *
#*   Copyright 2020 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************
ROOTDIR = $(abspath ..)

-include $(ROOTDIR)/Makefile.config
-include $(ROOTDIR)/stdlib/StdlibModules
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

include Makefile.docfiles

DOC_COMPILERLIBS_DIRS= $(addprefix $(ROOTDIR)/, \
parsing utils typing bytecomp driver file_formats lambda\
)

DOC_STDLIB_DIRS = $(addprefix $(ROOTDIR)/, stdlib \
	otherlibs/str \
	otherlibs/$(UNIXLIB) otherlibs/dynlink \
	otherlibs/systhreads\
)

all: html pdf man
.PHONY:all
.SUFFIXES:

.PHONY: setup
setup:
	$(MKDIR) build build/libref build/compilerlibref
	$(MKDIR) html html/libref html/compilerlibref
	$(MKDIR) man man/libref man/compilerlibref
	$(MKDIR) latex latex/libref latex/compilerlibref
	$(MKDIR) texi

pdf: latex/alldoc.pdf | setup
man: | setup
html: | setup
latex/alldoc.pdf: latex/stdlib_input.tex latex/compilerlibs_input.tex | latex/ifocamldoc.tex

Compiler_libs.mld: Compiler_libs.pre.mld
	cp $< $@ && echo "{!modules:$(compilerlibref_C)}" >> $@

latex/ifocamldoc.tex: $(ROOTDIR)/Makefile.config

latex/alldoc.tex:alldoc.tex
	cp $< $@

ifeq ($(DOCUMENTATION_MODE),odoc)
  include Makefile.odoc
else
  include Makefile.ocamldoc
endif


clean:
	rm -rf build man html latex texi
