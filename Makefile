build:
	ocamlfind ocamlc -c -package js_of_ocaml ojs.mli ojs.ml

	ocamlfind ocamlc -package compiler-libs.bytecomp -linkpkg -o gen_js_iface.exe gen_js_iface.ml

	./gen_js_iface.exe test_js.mli > test_js.ml

	ocamlfind ocamlc -c -package js_of_ocaml test_js.mli test_js.ml

	ocamlfind ocamlc -package js_of_ocaml -linkpkg -o main.exe ojs.cmo test_js.cmo main.ml
	js_of_ocaml -o main.js main.exe

clean:
	rm -f *~ *.exe *.cm* main.js
