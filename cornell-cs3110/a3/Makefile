test:
	ocamlbuild -pkgs oUnit,str,unix test_main.byte && ./test_main.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f data.mli.orig engine.mli.orig test_data.mli.orig test_engine.mli.orig
