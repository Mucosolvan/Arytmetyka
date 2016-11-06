test: arytmetyka.cmo tests.cmo
	ocamlc -o test arytmetyka.cmo tests.cmo

arytmetyka.cmo: arytmetyka.ml arytmetyka.cmi
	ocamlc -c arytmetyka.ml

arytmetyka.cmi: arytmetyka.mli
	ocamlc -c arytmetyka.mli

tests.cmo: tests.ml arytmetyka.cmi
	ocamlc -c tests.ml
clean:
	rm -f *.cmi *.cmo
