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
ROOTDIR = ..
SRC = $(ROOTDIR)

-include $(SRC)/Makefile.config
-include $(ROOTDIR)/stdlib/StdlibModules


PARSING_MLIS := $(notdir $(wildcard $(SRC)/parsing/*.mli))
UTILS_MLIS := $(notdir $(wildcard $(SRC)/utils/*.mli))
STR_MLIS := str.mli
UNIX_MLIS := unix.mli unixLabels.mli
DYNLINK_MLIS := dynlink.mli
THREAD_MLIS := thread.mli condition.mli mutex.mli event.mli threadUnix.mli
DRIVER_MLIS := pparse.mli

COMPILERLIBS_MLIS=\
  $(PARSING_MLIS) \
  $(UTILS_MLIS) \
  $(DRIVER_MLIS)

DOC_COMPILERLIBS_DIRS= $(addprefix $(ROOTDIR)/, \
parsing utils typing bytecomp driver file_formats lambda\
)

DOC_STDLIB_DIRS = $(addprefix $(ROOTDIR)/, stdlib \
	otherlibs/str \
	otherlibs/$(UNIXLIB) otherlibs/dynlink \
	otherlibs/systhreads\
)

vpath %.cmti $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)
vpath %.cmt $(ROOTDIR)/stdlib

STDLIB=$(filter-out stdlib__pervasives, $(STDLIB_MODULES))

STDLIB_CMTIS=$(STDLIB:%.mli=%.cmti)
STDLIB_ODOC_BASE=$(STDLIB:%=%.odoc)
STDLIB_ODOC=$(STDLIB_ODOC_BASE) stdlib__pervasives.odoc
ALL_ODOCL=$(STDLIB_MODULES:%=%.odocl) \
	$(UNIX_MLIS:%.mli=%.odocl) \
	$(DYNLINK_MLIS:%.mli=%.odocl) \
	$(THREAD_MLIS:%.mli=%.odocl) \
	$(STR_MLIS:%.mli=%.odocl) \
	$(COMPILERLIBS_MLIS:%.mli=%.odocl)


ALL_HTML=$(addprefix html/stdlib/, $(ALL_ODOCL:%.odocl=%/index.html))
ALL_LATEX=$(addprefix latex/package-stdlib-module-, $(ALL_ODOCL:%.odocl=%.tex))
ALL_MAN=$(addprefix man/, $(ALL_ODOCL:%.odocl=%.3o))

odoc=odoc

ALL: html latex man
.PHONY:ALL

.SUFFIXES:

html: $(ALL_HTML) html/odoc.css
latex: latex/manual.pdf

latex/manual.pdf: $(ALL_LATEX) latex/manual.tex
	cd latex && pdflatex manual.tex

man: $(ALL_MAN)

html/odoc.css:
	odoc support-files --output-dir html

stdlib__pervasives.odoc: stdlib__pervasives.cmt
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package stdlib $< -o $@

$(STDLIB_ODOC_BASE):%.odoc:%.cmti
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package stdlib $< -o $@

$(UNIX_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package unix $< -o $@

$(DYNLINK_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package dynlink $< -o $@

$(THREAD_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package thread $< -o $@

$(COMPILERLIBS_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package compilerlibs $< -o $@

$(STR_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$(odoc) $(DOC_STDLIB_INCLUDES) compile --package str $< -o $@

$(ALL_ODOCL):%.odocl:%.odoc barrier
	$(odoc) link $(DOC_STDLIB_INCLUDES) -I . $<

barrier:$(ALL_ODOCl:%.odocl:%.odoc)
	touch barrier


$(ALL_HTML):html/stdlib/%/index.html:%.odocl
	  $(odoc) html-generate --output-dir html  $<

$(ALL_LATEX):latex/package-stdlib-module-%.tex:%.odocl
	  $(odoc) latex-generate --output-dir latex  $<

$(ALL_MAN):man/%.3o:%.odocl
	  $(odoc) man-generate --output-dir man  $<


clean:
	rm barrier *.odoc *.odocl latex/package-*.tex \
	man/stdlib/*; rm -r html/* ;\
	rm latex/manual.pdf latex/manual.log latex/manual.out \
	latex/manual.aux latex/output.aux
