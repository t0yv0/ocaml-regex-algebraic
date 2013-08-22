clean:
	ocamlbuild -clean

test:
	ocamlbuild test.native;	./test.native; echo ""

