domain_simple.exe: domain_simple.ml
	ocamlopt $< -o $@

domain_hb.exe: domain_hb.ml
	ocamlopt $< -o $@

mp_queue.exe: mp_queue.ml
	ocamlopt -I +unix unix.cmxa $< -o $@

domain_queue.exe: domain_queue.ml
	ocamlopt $< -o $@

clean:
	rm mp_queue.{exe,o,cm*}
