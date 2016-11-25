build:
	ocamlbuild -pkgs yojson,cohttp.lwt,str -tag bin-annot serverhttp.byte

start:
	ocamlbuild -pkgs yojson,cohttp.lwt,str -tag bin-annot serverhttp.byte && ./serverhttp.byte

clean:
	ocamlbuild -clean
