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


-include $(ROOTDIR)/stdlib/StdlibModules

STDLIB=$(filter-out stdlib__pervasives, $(STDLIB_MODULES))
STDLIB_MLI0 = $(STDLIB:%=$(SRC)/stdlib/%.cmti)
ALL_CMTIS=$(STDLIB_MLI0:%.mli=%.cmti)
T_ODOC=$(STDLIB:%=%.odoc)
ALL_ODOC=$(T_ODOC) stdlib__pervasives.odoc
ALL_ODOCL=$(STDLIB_MODULES:%=%.odocl)

ALL_HTML=$(addprefix html/stdlib/, $(STDLIB_MODULES:%=%/index.html))
ALL_LATEX=$(addprefix latex/package-stdlib-module-, $(STDLIB_MODULES:%=%.tex))
ALL_MAN=$(addprefix man/, $(STDLIB_MODULES:%=%.3o))

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

stdlib__pervasives.odoc: $(ROOTDIR)/stdlib/stdlib__pervasives.cmt
	$(odoc) $(DOC_ALL_INCLUDES) compile --package stdlib $< -o $@

$(T_ODOC):%.odoc:$(SRC)/stdlib/%.cmti
	$(odoc) $(DOC_ALL_INCLUDES) compile --package stdlib $< -o $@

$(ALL_ODOCL):%.odocl:%.odoc barrier
	$(odoc) link $(DOC_ALL_INCLUDES) -I . $<

barrier:$(ALL_ODOC)
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
