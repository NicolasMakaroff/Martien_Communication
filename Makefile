all:

	ocamlc -c projet_IPF.mli projet_IPF.ml

	ocamlc -o projet projet_IPF.cmo



clean:
	rm -f *.cm[io] projet


