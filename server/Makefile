build:
	ocamlbuild -pkgs yojson,cohttp.lwt,str -tag bin-annot -tag thread serverhttp.byte

start:
	ocamlbuild -use-ocamlfind -pkgs yojson,cohttp.lwt,str -tag bin-annot -tag thread serverhttp.byte && ./serverhttp.byte

clean:
	ocamlbuild -clean
