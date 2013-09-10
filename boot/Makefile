

all: import_hump

import_hump: import_hump.ml
	ocamlfind ocamlopt -linkpkg -o $@ -package camomile,netstring,rdf,mysql $^