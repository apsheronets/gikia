PACKAGES = amall,lwt,lwt.extra,extlib,calendar,polebrush,polebrush.html,textile,textile.html,camlp4.lib
emls = $(wildcard views/*.eml)
views = $(emls:.eml=.ml)
revised_files = simplexmlparser.ml init.ml gikia.ml
original_files = $(views) utils.ml xmllexer.ml routes.ml views.ml highlight.ml io.ml parsercomb.ml vcs.ml markup.ml atom.ml cache.ml
ocamllex_files = xmllexer.mll
all_files = $(views) $(revised_files) $(original_files)

NAME = gikia
CAMLC   = ocamlfind ocamlc   -g -I views/ -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt -g -I views/ -thread $(LIB)
CAMLDOC = ocamlfind ocamldoc -I views/ -thread $(LIB)
CAMLDEP = ocamlfind ocamldep -I views/
LIB = -package $(PACKAGES)
PP = -pp camlp4r

revised_optobjs  = $(revised_files:.ml=.cmx)
original_optobjs = $(original_files:.ml=.cmx)

revised_objs  = $(revised_files:.ml=.cmo)
original_objs = $(original_files:.ml=.cmo)

OBJS    = $(all_files:.ml=.cmo)
OPTOBJS = $(all_files:.ml=.cmx)

all: $(NAME) $(NAME).opt

$(NAME): $(OBJS)
	$(CAMLC) -linkpkg -o $@ `ocamldep-sorter < .depend $^`

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -linkpkg -o $@ `ocamldep-sorter < .depend $^`

highlight.ml: highlight.stub.ml
	ln -f highlight.stub.ml $@

.SUFFIXES: .ml .mli .mll .cmo .cmi .cmx .eml

.PHONY: doc

.mll.ml:
	ocamllex $<
.eml.ml:
	ecaml -d -p 'Buffer.add_string buf' -esc-p 'Utils.esc_to_buf buf' $<

$(original_objs): %.cmo: %.ml
	$(CAMLC) -c $<
$(original_files:.ml=.cmi): %.cmi: %.mli
	$(CAMLC) -c $<
$(original_optobjs): %.cmx: %.ml
	$(CAMLOPT) -c $<

$(revised_objs): %.cmo: %.ml
	$(CAMLC) $(PP) -c $<
$(revised_files:.ml=.cmi): %.cmi: %.mli
	$(CAMLC) $(PP) -c $<
$(revised_optobjs): %.cmx: %.ml
	$(CAMLOPT) $(PP) -c $<

doc: $(OBJS) $(original_files) $(revised_files)
	-mkdir -p doc
	mkdir -p temp temp/views/
	cp *.ml* temp/
	cp views/*.ml* temp/views/
	for file in $(revised_files); do \
	camlp4r $$file -o temp/$$file ; \
	done
	$(CAMLDOC) -html -keep-code -d doc/ $(addprefix temp/,$(revised_files) $(original_files))
	rm -r temp

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~
	-rm -f views/*.cm[ioxa] views/*.cmx[as] views/*.o views/*.a *~
	-rm -f .depend
	-rm -rf doc
	-rm -f $(views)
	-rm -f highlight.ml
	-rm -f $(NAME) $(NAME).opt

depend: .depend

.depend: $(all_files)
	$(CAMLDEP) $(PP) $(LIB) $(revised_files:.ml=.mli) $(revised_files) > .depend
	$(CAMLDEP) $(LIB) $(original_files:.ml=.mli) $(original_files) >> .depend

FORCE:

-include .depend
