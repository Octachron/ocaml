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

DOC_STDLIB_INCLUDES=$(addprefix -I , $(DOC_STDLIB_DIRS))

DOC_ALL_INCLUDES = \
	$(DOC_STDLIB_INCLUDES) \
	$(addprefix -I ,$(DOC_COMPILERLIBS_DIRS))

vpath %.cmti $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)
vpath %.cmt $(ROOTDIR)/stdlib

define sort
$(shell $(BEST_OCAMLDEP) -sort $(1))
endef
define capitalize
$(shell echo $(1) | sed "s/\<./\U&/g")
endef

.PHONY:debug-%
debug-%:
	$(info $($*))

str_MLIS := str.mli
unix_MLIS := unix.mli unixLabels.mli
dynlink_MLIS := dynlink.mli
thread_MLIS := \
	thread.mli condition.mli mutex.mli event.mli \
	threadUnix.mli semaphore.mli

STDLIB=$(filter-out stdlib__pervasives, $(STDLIB_MODULES))

ifeq ($(DOCUMENTATION_MODE),odoc)
stdlib_UNPREFIXED=$(STDLIB)
else
stdlib_UNPREFIXED=$(STDLIB:stdlib__%=%)
endif
libref= \
	$(stdlib_UNPREFIXED) \
	$(str_MLIS:%.mli=%) \
	$(unix_MLIS:%.mli=%) \
	$(dynlink_MLIS:%.mli=%) \
	$(thread_MLIS:%.mli=%)
libref_EXTRA=stdlib__pervasives
libref_TEXT=Ocaml_operators
libref_C=$(call capitalize,$(libref) $(libref_EXTRA))


PARSING_MLIS := $(filter-out camlinternal%, \
	$(call sort, $(notdir $(wildcard $(ROOTDIR)/parsing/*.mli)))\
)
UTILS_MLIS := $(call sort, $(notdir $(wildcard $(ROOTDIR)/utils/*.mli)))
DRIVER_MLIS := pparse.mli

compilerlibref_MLIS=\
  $(PARSING_MLIS) \
  $(UTILS_MLIS) \
  $(DRIVER_MLIS)
compilerlibref=$(compilerlibref_MLIS:%.mli=%)
compilerlibref_TEXT=Compiler_libs
compilerlibref_C=$(call capitalize,$(compilerlibref))

ALL_LIBREF= $(libref_TEXT:%=libref/%) $(libref:%=libref/%)
ALL_COMPILERLIBREF= \
	$(compilerlibref_TEXT:%=compilerlibref/%) \
	$(compilerlibref:%=compilerlibref/%)
ALL_DOC= $(ALL_LIBREF) $(ALL_COMPILERLIBREF)
ALL_MAN= $(ALL_DOC:%=build/%.3o.witness)
ALL_LATEX= $(ALL_DOC:%=build/%.tex.witness)

all: html pdf man
.PHONY:ALL
.SUFFIXES:

pdf: latex/manual.pdf
latex/manual.pdf: latex/stdlib_input.tex latex/compilerlibs_input.tex \
	| latex/ifocamldoc.tex

latex/ifocamldoc.tex: $(ROOTDIR)/Makefile.config


compilerlibref/Compiler_libs.mld: compilerlibref/Compiler_libs.pre.mld
	cp $< $@ && echo "{!modules:$(compilerlibref_C)}" >> $@


ifeq ($(DOCUMENTATION_MODE),odoc)


odoc=odoc

define page_name
	$(dir $1)page-$(notdir $1)
endef
test:
	$(info $(dir build/libref/a.doc))
	$(info $(call pkg_name,build/libref/a.doc))

latex/ifocamldoc.tex:
	echo '\\newif\ifocamldoc\ocamldocfalse' > $@

stdlib_INPUT=$(foreach module,\
$(filter-out stdlib camlinternal%, $(STDLIB:stdlib__%=%)),\
\\input{libref/Stdlib.$(call capitalize,$(module)).tex}\
)
latex/stdlib_input.tex:
	echo $(stdlib_INPUT)> $@

latex/compilerlibs_input.tex:
	echo $(compilerlibref_C:%=\\input{compilerlibref/%})> $@


$(libref_TEXT:%=build/libref/page-%.odoc):
build/libref/page-%.odoc:libref/%.mld
	$(odoc) compile -I build/libref --package libref $< -o $@

$(compilerlibref_TEXT:%=build/compilerlibref/page-%.odoc):\
build/compilerlibref/page-%.odoc:compilerlibref/%.mld
	$(odoc) compile -I build/libref --package compilerlibref $< -o $@

$(libref:%=build/libref/%.odoc):build/libref/%.odoc:%.cmti
	$(odoc) compile -I build/libref  --package $(call pkg_name,$@) $< -o $@
$(libref_EXTRA:%=build/libref/%.odoc):build/libref/%.odoc:%.cmt
	$(odoc) compile -I build/libref --package libref $< -o $@

$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:%.cmti $(libref:%=build/libref/%.odoc)
	$(odoc) compile -I build/libref -I build/compilerlibref \
	--package compilerlibref $< -o $@

latex/manual.pdf: $(ALL_LATEX) latex/manual.tex \
latex/stdlib_input.tex latex/compilerlibs_input.tex
	cd latex && pdflatex manual.tex
man: $(ALL_MAN)

html/odoc.css:
	$(odoc) support-files --output-dir html

ALL_TEXT = $(libref_TEXT:%=libref/%) $(compilerlibref_TEXT:%=compilerlibref/%)
ALL_PAGE_TEXT=$(foreach mld,$(ALL_TEXT),$(call page_name,$(mld)))
ALL_UNITS=$(compilerlibref:%=compilerlibref/%) $(libref:%=libref/%)
ALL_PAGED_DOC= $(ALL_UNITS) $(ALL_PAGE_TEXT)
ALL_HTML= $(ALL_PAGED_DOC:%=build/%.html.witness)

html: $(ALL_HTML) html/odoc.css

$(ALL_UNITS:%=build/%.odocl):%.odocl:%.odoc \
	| $(ALL_PAGED_DOC:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $<

$(ALL_PAGE_TEXT:%=build/%.odocl):%.odocl:%.odoc \
	| $(ALL_PAGED_DOC:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $<

$(ALL_HTML):%.html.witness:%.odocl
	$(odoc) html-generate --output-dir html  $<
	touch $@

not_stdlib=$(filter-out libref/stdlib,$(ALL_UNITS))
$(not_stdlib:%=build/%.tex.witness):\
%.tex.witness:%.odocl
	$(odoc) latex-generate --with-children=true --output-dir latex  $<
	touch $@

build/libref/stdlib.tex.witness: build/libref/stdlib.odocl
	$(odoc) latex-generate --with-children=false --output-dir latex  $<
	touch $@

$(ALL_UNITS:%=build/%.3o.witness):%.3o.witness:%.odocl
	$(odoc) man-generate --output-dir man  $<
	touch $@


$(libref_TEXT:%=build/libref/%.3o.witness):
build/libref/%.3o.witness: build/libref/page-%.odocl
	$(odoc) man-generate --output-dir man  $<
	touch $@

$(compilerlibref_TEXT:%=build/compilerlibref/%.3o.witness):\
build/compilerlibref/%.3o.witness:build/compilerlibref/page-%.odocl
	$(odoc) man-generate --output-dir man  $<
	touch $@

$(libref_TEXT:%=build/libref/%.tex.witness):\
build/libref/%.tex.witness:build/libref/page-%.odocl
	$(odoc) latex-generate --output-dir latex  $<
	touch $@

$(compilerlibref_TEXT:%=build/compilerlibref/%.tex.witness):\
build/compilerlibref/%.tex.witness:build/compilerlibref/page-%.odocl
	$(odoc) latex-generate --output-dir latex  $<
	touch $@

else

vpath %.mli $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)

man: man/Stdlib.3o
html: html/libref/Stdlib.html html/compilerlibref/Compiler_libs.html

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

$(libref:%=build/libref/%.odoc):build/libref/%.odoc:%.mli
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	-pp \
    "$(AWK) -v ocamldoc=true -f $(ROOTDIR)/stdlib/expand_module_aliases.awk" \
	$(DOC_STDLIB_INCLUDES) $< -dump  $@

$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:%.mli
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	$(DOC_ALL_INCLUDES) $< -dump  $@

$(compilerlibref_TEXT:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:%.mld
	$(OCAMLDOC_RUN) $(DOC_ALL_INCLUDES) -text $< -dump  $@

$(libref_TEXT:%=build/libref/%.odoc):build/libref/%.odoc:%.mld
	$(OCAMLDOC_RUN) $(DOC_STDLIB_INCLUDES) -text $< -dump  $@

ALL_COMPILED_DOC=$(ALL_DOC:%=build/%.odoc)
man/Stdlib.3o: $(ALL_COMPILED_DOC)
	$(MKDIR) man
	$(OCAMLDOC_RUN) -man -d man -man-mini \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

html/libref/Stdlib.html: $(ALL_LIBREF:%=build/%.odoc)
	$(MKDIR) -p html/libref
	$(OCAMLDOC_RUN) -html -d html/libref \
	-charset="utf8" \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_LIBREF:%=build/%.odoc))

html/compilerlibref/Compiler_libs.html: $(ALL_COMPILERLIBREF:%=build/%.odoc)
	$(MKDIR) -p html/compilerlibref
	$(OCAMLDOC_RUN) -html -d html/compilerlibref \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml compiler library" \
	-charset="utf8" \
	-intro compiler_libs.mld \
	$(addprefix -load , $(ALL_COMPILERLIBREF:%=build/%.odoc))

texi/stdlib.texi: $(ALL_COMPILED_DOC)
	$(MKDIR) texi
	$(OCAMLDOC_RUN) -texi -o $@ \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

latex/Stdlib.tex: $(ALL_COMPILED_DOC)
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
	$(addprefix -load ../, $(ALL_COMPILED_DOC))

latex/manual.pdf: latex/Stdlib.tex
	cd latex && TEXINPUTS=$${TEXINPUTS}:$(ROOTDIR)/ocamldoc pdflatex manual
	cd latex && TEXINPUTS=$${TEXINPUTS}:$(ROOTDIR)/ocamldoc pdflatex manual

stdlib_INPUT=$(foreach module,\
$(filter-out stdlib.mli camlinternal%,$(stdlib_UNPREFIXED)),\
\\input{$(call capitalize,$(module)).tex}\
)
latex/stdlib_input.tex:
	echo $(stdlib_INPUT)> $@

compilerlibs_INPUT=$(foreach module,\
$(filter-out camlinternal%,$(compilerlibref)),\
\\input{$(call capitalize,$(module)).tex})
latex/compilerlibs_input.tex:
	echo $(compilerlibs_INPUT)> $@

endif


clean:
	rm -f build/*/*.odoc build/*/*.odocl latex/*/*.tex \
	build/*/*witness ;\
	rm -rf man/*; rm -rf html/* ;\
	rm -f latex/*.pdf latex/*.log latex/*.out \
	latex/*_input.tex latex/*.aux
