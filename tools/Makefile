

all: hump_gen import_hump

hump_gen: hump_rdf.cmx hump_gen.ml
	ocamlfind ocamlopt -linkpkg -o $@ -rectypes -package xtmpl,rdf $^

import_hump: hump_rdf.cmx import_hump.ml
	ocamlfind ocamlopt -linkpkg -o $@ -package camomile,netstring,rdf,mysql $^

hump_rdf.cmx: hump_rdf.ml
	ocamlfind ocamlopt -package rdf -c $^