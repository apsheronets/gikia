PACKAGES = amall,lwt,lwt.extra,extlib,calendar,polebrush,polebrush.html,textile,textile.html,magic,camlp4.lib
#emls = $(wildcard views/*.eml)
#emls = $(wildcard *.eml)
emls = header_table.eml article.eml index.eml history.eml full_history.eml view_full_change.eml error.eml layout.eml view_change.eml
views = $(emls:.eml=.ml)
revised_files = simplexmlparser.ml init.ml gikia.ml
original_files = utils.ml xmllexer.ml routes.ml views.ml highlight.ml io.ml parsercomb.ml vcs.ml $(views) markup.ml atom.ml
ocamllex_files = xmllexer.mll
all_files = utils.ml xmllexer.ml simplexmlparser.ml io.ml highlight.ml markup.ml init.ml parsercomb.ml vcs.ml layout.ml routes.ml views.ml header_table.ml article.ml index.ml history.ml full_history.ml view_full_change.ml error.ml view_change.ml atom.ml gikia.ml

NAME = gikia
CAMLC   = ocamlfind ocamlc   -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt -thread $(LIB)
CAMLDEP = ocamlfind ocamldep
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
