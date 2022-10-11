domain_simple.exe: domain_simple.ml
	ocamlopt $< -o $@

domain_hb.exe: domain_hb.ml
	ocamlopt $< -o $@
