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

DOC_COMPILERLIBS_DIRS= $(addprefix $(ROOTDIR)/, \
parsing utils typing bytecomp driver file_formats lambda\
)

DOC_STDLIB_DIRS = $(addprefix $(ROOTDIR)/, stdlib \
	otherlibs/str \
	otherlibs/$(UNIXLIB) otherlibs/dynlink \
	otherlibs/systhreads\
)

DOC_ALL_INCLUDES = \
	$(addprefix -I , $(DOC_STDLIB_DIRS) \
	$(DOC_COMPILERLIBS_DIRS)) \
	-I .

vpath %.cmti $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)
vpath %.cmt $(ROOTDIR)/stdlib

OTHERLIBS_PACKAGES= unix dynlink str thread
LIBS_PACKAGES=stdlib $(OTHERLIBS_PACKAGES)
PACKAGES=$(LIBS_PACKAGES) compilerlibs

str_MLIS := str.mli
unix_MLIS := unix.mli unixLabels.mli
dynlink_MLIS := dynlink.mli
thread_MLIS := \
	thread.mli condition.mli mutex.mli event.mli \
	threadUnix.mli semaphore.mli

PARSING_MLIS := $(filter-out camlinternal%, \
	$(notdir $(wildcard $(ROOTDIR)/parsing/*.mli))\
)
UTILS_MLIS := $(notdir $(wildcard $(ROOTDIR)/utils/*.mli))
DRIVER_MLIS := pparse.mli

compilerlibs_MLIS=\
  $(PARSING_MLIS) \
  $(UTILS_MLIS) \
  $(DRIVER_MLIS)
compilerlibs_TEXT= page-compiler_libs.odocl

STDLIB=$(filter-out stdlib__pervasives camlinternal%, $(STDLIB_MODULES))

stdlib_MLIS=$(STDLIB:%=%.mli)
stdlib_EXTRA=stdlib__pervasives.odocl
stdlib_TEXT=page-Ocaml_operators.odocl
ALL_ODOCL=$(foreach package, $(PACKAGES),\
$($(package)_MLIS:%.mli=%.odocl) \
$($(package)_EXTRA)\
$($(package)_TEXT)\
)

define compile_package_
$$($(1)_MLIS:%.mli=%.odoc):%.odoc:%.cmti
	$$(odoc) compile $$(DOC_ALL_INCLUDES)  --package $(1) $$< -o $$@
$$($(1)_TEXT:page-%.odocl=page-%.odoc):page-%.odoc:%.mld
	$$(odoc) compile $$(DOC_ALL_INCLUDES) --package $(1) $$< -o $$@
$$($(1)_EXTRA:%.odocl=%.odoc):%.odoc:%.cmt
	$$(odoc) compile $$(DOC_ALL_INCLUDES) --package $(1) $$< -o $$@
endef

define compile_package
$(eval $(call compile_package_,$1))
endef

ALL_HTML=$(ALL_ODOCL:%.odocl=%.html.witness)
ALL_MAN=$(ALL_ODOCL:%.odocl=%.man.witness)
ALL_LATEX=$(ALL_ODOCL:%.odocl=%.tex.witness)

odoc=odoc

DOC_MODE=OCAMLDOC


ALL: html latex man
.PHONY:ALL

.SUFFIXES:

latex: latex/manual.pdf

latex/manual.pdf: latex/stdlib_input.tex latex/compilerlibs_input.tex \
	latex/ifocamldoc.tex

.PHONY: latex/ifocamldoc.tex

define capitalize
$(shell echo $(1) | sed "s/\<./\U&/g")
endef

compiler_libs.mld: compiler_libs.pre.mld
	cp $< $@ && echo "{!modules:$(compilerlibs_MODULES)}" >> $@


ifeq ($(DOCUMENTATION_MODE),odoc)

latex/ifocamldoc.tex:
	echo '\\newif\ifocamldoc\ocamldocfalse' > $@

stdlib_INPUT=$(foreach module,\
$(filter-out stdlib.mli camlinternal%, $(stdlib_MLIS:stdlib__%.mli=%)),\
\\input{stdlib/Stdlib.$(call capitalize,$(module)).tex}\
)
latex/stdlib_input.tex:
	echo $(stdlib_INPUT)> $@

compilerlibs_INPUT=$(foreach module,\
$(compilerlibs_MLIS:%.mli=%),\
\\input{compilerlibs/$(call capitalize,$(module)).tex})
latex/compilerlibs_input.tex:
	echo $(compilerlibs_INPUT)> $@


html: $(ALL_HTML) html/odoc.css
latex/manual.pdf: $(ALL_LATEX) latex/manual.tex \
latex/stdlib_input.tex latex/compilerlibs_input.tex
	cd latex && pdflatex manual.tex
man: $(ALL_MAN)

html/odoc.css:
	$(odoc) support-files --output-dir html


compilerlibs_MODULES=$(foreach module,\
$(compilerlibs_MLIS:%.mli=%),$(call capitalize, $(module)))


$(ALL_ODOCL):%.odocl:%.odoc | $(ALL_ODOCl:%.odocl=%.odoc)
	$(odoc) link $(DOC_ALL_INCLUDES) $<

$(foreach package,$(PACKAGES), $(call compile_package,$(package)))


$(ALL_HTML):%.html.witness:%.odocl
	$(odoc) html-generate --output-dir html  $<
	touch $@

$(ALL_LATEX):%.tex.witness:%.odocl
	$(odoc) latex-generate --with-children=false --output-dir latex  $<
	touch $@

$(ALL_MAN):%.man.witness:%.odocl
	$(odoc) man-generate --output-dir man  $<
	touch $@
else

vpath %.mli $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)

man: man/Stdlib.3o
html: html/stdlib/Stdlib.html html/compilerlibs/Compiler_libs.html

latex/ifocamldoc.tex:
	echo '\\newif\ifocamldoc\ocamldoctrue' > $@


OCAMLRUN ?= $(ROOTDIR)/boot/ocamlrun$(EXE)
OCAMLDOC=$(ROOTDIR)/ocamldoc/ocamldoc$(EXE)
OCAMLDOC_OPT=$(ROOTDIR)/ocamldoc/ocamldoc.opt$(EXE)
LOCAL_OCAMLDOC=
ifeq "$(UNIX_OR_WIN32)" "unix"
  ifeq "$(TARGET)" "$(HOST)"
    ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
      OCAMLDOC_RUN_BYTE=$(OCAMLRUN) -I $(ROOTDIR)/otherlibs/$(UNIXLIB) \
	-I $(ROOTDIR)/otherlibs/str $(OCAMLDOC)
    else
# if shared-libraries are not supported, unix.cma and str.cma
# are compiled with -custom, so ocamldoc also uses -custom,
# and (ocamlrun ocamldoc) does not work.
      OCAMLDOC_RUN_BYTE=$(OCAMLDOC)
    endif
  else
    OCAMLDOC_RUN_BYTE=$(OCAMLRUN) $(OCAMLDOC)
  endif
else # Windows
  OCAMLDOC_RUN_BYTE = \
   CAML_LD_LIBRARY_PATH=\
	"$(ROOTDIR)/otherlibs/win32unix;$(ROOTDIR)/otherlibs/str" \
	$(OCAMLRUN) $(OCAMLDOC)
endif

OCAMLDOC_RUN_OPT=$(OCAMLDOC_OPT)

ifeq "$(wildcard $(OCAMLDOC_OPT))" ""
  OCAMLDOC_RUN=$(OCAMLDOC_RUN_BYTE)
else
  OCAMLDOC_RUN=$(OCAMLDOC_RUN_OPT)
endif

DOC_LIBS_MLIS= $(stdlib_MLIS:stdlib__%=%) \
	$(foreach package,$(OTHERLIBS_PACKAGES), $($(package)_MLIS))
DOC_LIBS_TEXT= $(foreach package,$(LIBS_PACKAGES), $($(package)_TEXT))
DOC_ALL_MLIS = $(DOC_LIBS_MLIS) $(compilerlibs_MLIS)
DOC_ALL_TEXT = $(DOC_LIBS_TEXT) $(compilerlibs_TEXT)

DOC_LIBS = $(DOC_LIBS_MLIS:%.mli=%.odoc) $(DOC_ALL_TEXT:page-%.odocl=%.odoc)
DOC_COMPILERLIBS= $(compilerlibs_MLIS:%.mli=%.odoc) \
	$(compilerlibs_TEXT:page-%.odocl=%.odoc)


$(DOC_ALL_MLIS:%.mli=%.odoc):%.odoc:%.mli
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	-pp \
    "$(AWK) -v ocamldoc=true -f $(ROOTDIR)/stdlib/expand_module_aliases.awk" \
	$(DOC_ALL_INCLUDES) $< -dump  $@

$(DOC_ALL_TEXT:page-%.odocl=%.odoc):%.odoc:%.mld
	$(OCAMLDOC_RUN) $(DOC_ALL_INCLUDES) -text $< -dump  $@


man/Stdlib.3o: $(DOC_ALL)
	$(MKDIR) man
	$(OCAMLDOC_RUN) -man -d man -man-mini \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(DOC_ALL))

html/stdlib/Stdlib.html: $(DOC_LIBS)
	$(MKDIR) -p html/stdlib
	$(OCAMLDOC_RUN) -html -d html/stdlib \
	-charset="utf8" \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(DOC_LIBS))

html/compilerlibs/Compiler_libs.html: $(DOC_COMPILERLIBS)
	$(MKDIR) -p html/compilerlibs
	$(OCAMLDOC_RUN) -html -d html/compilerlibs \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml compiler library" \
	-charset="utf8" \
	-intro compiler_libs.mld \
	$(addprefix -load , $(DOC_COMPILERLIBS))

texi/stdlib.texi: $(DOC_ALL)
	$(MKDIR) texi
	$(OCAMLDOC_RUN) -texi -o $@ \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(DOC_ALL))

latex/Stdlib.tex: $(DOC_ALL)
	$(MKDIR) latex
	cd latex && $(OCAMLDOC_RUN) -latex \
	-hide Stdlib -lib Stdlib $(DOC_ALL_INCLUDES) \
	-sepfiles \
	-latextitle "1,subsection*" \
	-latextitle "2,subsubsection*" \
	-latex-type-prefix "TYP" \
	-latex-module-prefix "" \
	-latex-module-type-prefix "" \
	-latex-value-prefix "" \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load ../, $(DOC_ALL))

latex/manual.pdf: latex/Stdlib.tex
	cd latex && TEXINPUTS=$${TEXINPUTS}:$(ROOTDIR)/ocamldoc pdflatex manual
	cd latex && TEXINPUTS=$${TEXINPUTS}:$(ROOTDIR)/ocamldoc pdflatex manual

stdlib_INPUT=$(foreach module,\
$(filter-out stdlib.mli camlinternal%, $(stdlib_MLIS:stdlib__%.mli=%)),\
\\input{$(call capitalize,$(module)).tex}\
)
latex/stdlib_input.tex:
	echo $(stdlib_INPUT)> $@

compilerlibs_INPUT=$(foreach module,\
$(filter-out camlinternal%,$(compilerlibs_MLIS:%.mli=%)),\
\\input{$(call capitalize,$(module)).tex})
latex/compilerlibs_input.tex:
	echo $(compilerlibs_INPUT)> $@



endif


clean:
	rm *.odoc *.odocl latex/*/*.tex \
	*witness \
	rm -r man/*; rm -r html/* ;\
	rm latex/*.pdf latex/*.log latex/*.out \
	latex/*_input.tex latex/*.aux
