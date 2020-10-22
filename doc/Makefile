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

define sort
$(shell $(BEST_OCAMLDEP) -sort $(1))
endef
define capitalize
$(shell echo $(1) | sed "s/\<./\U&/g")
endef
define uncapitalize
$(shell echo $(1) | sed "s/\<./\L&/g")
endef
define capitalize_all
$(call capitalize,$(1))
endef

str_MLIS := str.mli
unix_MLIS := unix.mli unixLabels.mli
dynlink_MLIS := dynlink.mli
thread_MLIS := \
	thread.mli condition.mli mutex.mli event.mli \
	threadUnix.mli semaphore.mli

STDLIB=$(filter-out stdlib__pervasives, $(STDLIB_MODULES))
libref= \
	$(STDLIB) \
	$(str_MLIS:%.mli=%) \
	$(unix_MLIS:%.mli=%) \
	$(dynlink_MLIS:%.mli=%) \
	$(thread_MLIS:%.mli=%)
libref_EXTRA=stdlib__pervasives
libref_TEXT=Ocaml_operators
libref_C=$(call capitalize_all,$(libref) $(libref_EXTRA))
libref_ALL=$(libref_C) $(libref_TEXT:%=%)


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
compilerlibref_C=$(call capitalize_all,$(compilerlibref))
compilerlibref_ALL=$(compilerlibref_C) $(compilerlibref_TEXT:%=%)

ALL: html latex man
.PHONY:ALL
.SUFFIXES:

latex: latex/manual.pdf
latex/manual.pdf: latex/stdlib_input.tex latex/compilerlibs_input.tex \
	| latex/ifocamldoc.tex

.PHONY: latex/ifocamldoc.tex


Compiler_libs.mld: Compiler_libs.pre.mld
	cp $< $@ && echo "{!modules:$(compilerlibref_C)}" >> $@


ifeq ($(DOCUMENTATION_MODE),odoc)

odoc=odoc

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

$(libref:%=build/libref/%.odoc):build/libref/%.odoc:%.cmti
	$(odoc) compile -I build/libref  --package libref $< -o $@
$(libref_TEXT:%=build/libref/page-%.odoc):build/libref/page-%.odoc:%.mld
	$(odoc) compile -I build/libref --package libref $< -o page-$@
$(libref_EXTRA:%=build/libref/%.odoc):build/libref/%.odoc:%.cmt
	$(odoc) compile -I build/libref --package libref $< -o $@

$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:%.cmti
	$(odoc) compile -I build/libref -I build/compileref \
	--package compilerlibref $< -o $@
$(compilerlibref_TEXT:%=build/compilerlibref/page-%.odoc):\
build/compilerlibref/page-%.odoc:%.mld
	$(odoc) compile -I build/compilerlibref \
		--package compilerlibref $< -o $@

.SECONDEXPANSION:
$(libref_C:%=build/libref/%.odoc):\
build/libref/%.odoc:build/libref/$$(call uncapitalize,%).odoc
	cp $< $@

.SECONDEXPANSION:
$(compilerlibref_C:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:build/compilerlibref/$$(call uncapitalize, %).odoc
	cp $< $@

ALL_FILES=$(libref_C:%=libref/%) $(compilerlibref_C:%=compilerlibref/%)
ALL_TEXT= \
	$(libref_TEXT=libref/page-%) \
	$(compilerlibref_TEXT=compilerlibref/page-%)
ALL_HTML= \
	$(libref_C:%=html/libref/%/index.html) \
	$(compilerlibref_C:%=html/compilerlibref/%/index.html) \
	$(ALL_TEXT)
ALL_MAN= \
	$(libref_ALL:%=man/libref/%.3o) \
	$(compilerlibref_ALL:%=man/compilerlibref/%.3o)
ALL_LATEX= \
	$(libref_ALL:%=latex/libref/%.tex) \
	$(compilerlibref_ALL:%=latex/compilerlibref/%.tex)

html: $(ALL_HTML) html/odoc.css

latex/manual.pdf: $(ALL_LATEX) latex/manual.tex \
latex/stdlib_input.tex latex/compilerlibs_input.tex
	cd latex && pdflatex manual.tex
man: $(ALL_MAN)

html/odoc.css:
	$(odoc) support-files --output-dir html


$(ALL_FILES:%=build/%.odocl) $(ALL_TEXT:%=%.odocl) :%.odocl:%.odoc \
	| $(ALL_FILES:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $<


$(ALL_HTML):html/%/index.html:build/%.odocl
	$(odoc) html-generate --output-dir html  $<

latex/libref/Stdlib.tex: build/libref/Stdlib.odocl
	$(odoc) latex-generate --with-children=false --output-dir latex  $<

not_stdlib=$(filter-out libref/Stdlib,$(ALL_FILES))
$(not_stdlib:%=latex/%.tex):latex/%.tex:build/%.odocl
	$(odoc) latex-generate --with-children=true --output-dir latex  $<


$(ALL_FILES:%=man/%.3o):man/%.3o:build/%.odocl
	$(odoc) man-generate --output-dir man  $<

$(libref_TEXT:%=man/libref/%.3o): man/libref/%.3o: build/libref/page-%.odocl
	$(odoc) man-generate --output-dir man  $<

$(compilerlibref_TEXT:%=man/compilerlibref/%.3o):\
man/compilerlibref/%.3o:build/compilerlibref/page-%.odocl
	$(odoc) man-generate --output-dir man  $<

$(libref_TEXT:%=latex/libref/%.tex):\
latex/libref/%.tex:build/libref/page-%.odocl
	$(odoc) latex-generate --output-dir latex  $<

$(compilerlibref_TEXT:%=latex/compilerlibref/%.tex):\
latex/compilerlibref/%.tex:build/compilerlibref/page-%.odocl
	$(odoc) latex-generate --output-dir latex  $<

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

DOC_LIBS_MLIS= $(addsuffix .mli, $(libref:stdlib__%=%))
DOC_ALL_MLIS = $(DOC_LIBS_MLIS) $(compilerlibref:%=%.mli)
DOC_ALL_TEXT = $(libref_TEXT) $(compilerlibref_TEXT)

DOC_LIBS = \
	$(DOC_LIBS_MLIS:%.mli=build/libref/%.odoc) \
	$(libref_TEXT:%=build/libref/%.odoc)
DOC_COMPILERLIBS= \
	$(compilerlibref_TEXT:%=build/compilerlibref/%.odoc) \
	$(compilerlibref_MLIS:%.mli=build/compilerlibref/%.odoc)
DOC_ALL=$(DOC_LIBS) $(DOC_COMPILERLIBS)

$(DOC_LIBS_MLIS:%.mli=build/libref/%.odoc):build/libref/%.odoc:%.mli
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	-pp \
    "$(AWK) -v ocamldoc=true -f $(ROOTDIR)/stdlib/expand_module_aliases.awk" \
	$(DOC_ALL_INCLUDES) $< -dump  $@

$(compilerlibref_MLIS:%.mli=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:%.mli
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	$(DOC_ALL_INCLUDES) $< -dump  $@

$(compilerlibref_TEXT:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc:%.mld
	$(OCAMLDOC_RUN) $(DOC_ALL_INCLUDES) -text $< -dump  $@

$(libref_TEXT:%=build/libref/%.odoc):build/libref/%.odoc:%.mld
	$(OCAMLDOC_RUN) $(DOC_ALL_INCLUDES) -text $< -dump  $@


man/Stdlib.3o: $(DOC_ALL)
	$(MKDIR) man
	$(OCAMLDOC_RUN) -man -d man -man-mini \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(DOC_ALL))

html/libref/Stdlib.html: $(DOC_LIBS)
	$(MKDIR) -p html/libref
	$(OCAMLDOC_RUN) -html -d html/libref \
	-charset="utf8" \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(DOC_LIBS))

html/compilerlibref/Compiler_libs.html: $(DOC_COMPILERLIBS)
	$(MKDIR) -p html/compilerlibref
	$(OCAMLDOC_RUN) -html -d html/compilerlibref \
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

stdlib_SIMPLE=$(STDLIB:stdlib__%=%)
stdlib_INPUT=$(foreach module,\
$(filter-out stdlib.mli camlinternal%,$(stdlib_SIMPLE)),\
\\input{$(call capitalize,$(module)).tex}\
)
latex/stdlib_input.tex:
	echo $(stdlib_INPUT)> $@

compilerlibs_INPUT=$(foreach module,\
$(filter-out camlinternal%,$(compilerlibref_MLIS:%.mli=%)),\
\\input{$(call capitalize,$(module)).tex})
latex/compilerlibs_input.tex:
	echo $(compilerlibs_INPUT)> $@



endif


clean:
	rm -f *.odoc *.odocl latex/*/*.tex \
	*witness ;\
	rm -rf man/*; rm -rf html/* ;\
	rm -f latex/*.pdf latex/*.log latex/*.out \
	latex/*_input.tex latex/*.aux
