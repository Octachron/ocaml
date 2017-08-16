all:c/test.cmo

TOPDIR=$(abspath ../../..)
include $(TOPDIR)/Makefile.tools

clean:
	@rm **/*.cm*; rm -r build

build/a/a.cmo: a/m.ml
	@printf " ... testing 'typing-pack': " &&\
	cd a &&\
	$(OCAMLC) -c m.ml -o m.cmo -for-pack a &&\
        $(OCAMLC) -pack m.cmo -o a.cmo &&\
        cd ../ &&\
        mkdir -p build/a &&\
        cp a/a.* build/a/ \
	|| printf "failed(1)\n"

build/b/b.cmo: build/a/a.cmo b/n.ml
	@cd b &&\
        $(OCAMLC) -I ../a/ -c n.ml -o n.cmo -for-pack b &&\
        $(OCAMLC) -pack n.cmo -o b.cmo &&\
        cd ../ &&\
        mkdir -p build/b &&\
        cp b/b.* build/b/ \
	|| printf "failed(2)\n"

c/test.cmo: build/b/b.cmo c/test.ml
	@cd c &&\
        $(OCAMLC) -c -I ../build/a/ -I ../build/b/ test.ml -o test.cmo -for-pack c \
	1>/dev/null 2>../test.result \
	; cd .. && diff test.result test.reference>/dev/null \
	&& printf "passed\n" \
	|| printf "failed\n"
