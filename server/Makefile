build:
	ocamlbuild -pkgs yojson,cohttp.lwt,str serverhttp.native

ast:
	ocamlbuild -pkgs yojson,cohttp.lwt,str -tag bin-annot -tag thread serverhttp.native

start:
	ocamlbuild -use-ocamlfind -pkgs yojson,cohttp.lwt,str -tag bin-annot -tag thread serverhttp.native && ./serverhttp.native

clean:
	ocamlbuild -clean
