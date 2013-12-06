PACKAGES = amall,lwt,lwt.extra,extlib,calendar,polebrush,polebrush.html,textile,textile.html,magic,camlp4.lib
#emls = $(wildcard views/*.eml)
emls = views/header_table.eml views/article.eml views/index.eml views/history.eml views/full_history.eml views/view_full_change.eml views/error.eml views/layout.eml views/view_change.eml
views = $(emls:.eml=.ml)
revised_files = simplexmlparser.ml init.ml gikia.ml
original_files = utils.ml xmllexer.ml routes.ml views.ml highlight.ml io.ml parsercomb.ml vcs.ml $(views) markup.ml atom.ml
ocamllex_files = xmllexer.mll
all_files = utils.ml xmllexer.ml simplexmlparser.ml io.ml highlight.ml markup.ml init.ml parsercomb.ml vcs.ml views/layout.ml routes.ml views.ml views/header_table.ml views/article.ml views/index.ml views/history.ml views/full_history.ml views/view_full_change.ml views/error.ml views/view_change.ml atom.ml gikia.ml

NAME = gikia
CAMLC   = ocamlfind ocamlc   -I views/ -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt -I views/ -thread $(LIB)
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
	$(CAMLC) -linkpkg -o $@ $^

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -linkpkg -o $@ $^

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
