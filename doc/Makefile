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

PACKAGES=stdlib unix dynlink str compilerlibs thread
str_MLIS := str.mli
unix_MLIS := unix.mli unixLabels.mli
dynlink_MLIS := dynlink.mli
thread_MLIS := thread.mli condition.mli mutex.mli event.mli threadUnix.mli

PARSING_MLIS := $(notdir $(wildcard $(SRC)/parsing/*.mli))
UTILS_MLIS := $(notdir $(wildcard $(SRC)/utils/*.mli))
DRIVER_MLIS := pparse.mli

compilerlibs_MLIS=\
  $(PARSING_MLIS) \
  $(UTILS_MLIS) \
  $(DRIVER_MLIS)

STDLIB=$(filter-out stdlib__pervasives, $(STDLIB_MODULES))

stdlib_MLIS=$(STDLIB:%=%.mli)
stdlib_EXTRA=stdlib__pervasives.odocl
ALL_ODOCL=$(foreach package, $(PACKAGES),\
$($(package)_MLIS:%.mli=%.odocl) $($(package)_EXTRA))

define compile_package_
$$($(1)_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$$(odoc) $$(DOC_STDLIB_INCLUDES) compile --package $(1) $$< -o $$@
endef

define compile_package
$(eval $(call compile_package_,$1))
endef

ALL_HTML=$(ALL_ODOCL:%.odocl=%.html.witness)
ALL_MAN=$(ALL_ODOCL:%.odocl=%.man.witness)
ALL_LATEX=$(ALL_ODOCL:%.odocl=%.tex.witness)

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


$(ALL_ODOCL):%.odocl:%.odoc barrier
	$(odoc) link $(DOC_STDLIB_INCLUDES) -I . $<

barrier:$(ALL_ODOCl:%.odocl=%.odoc)
	touch $@

$(foreach package,$(PACKAGES), $(call compile_package,$(package)))


$(ALL_HTML):%.html.witness:%.odocl
	$(odoc) html-generate --output-dir html  $<
	touch $@

$(ALL_LATEX):%.tex.witness:%.odocl
	$(odoc) latex-generate --output-dir latex  $<
	touch $@


$(ALL_MAN):%.man.witness:%.odocl
	$(odoc) man-generate --output-dir man  $<
	touch $@

clean:
	rm barrier *.odoc *.odocl latex/package-*.tex \
	rm *witness \
	man/stdlib/*; rm -r html/* ;\
	rm latex/manual.pdf latex/manual.log latex/manual.out \
	latex/manual.aux latex/output.aux
