build:
	ocamlbuild -pkgs yojson,lwt,cohttp.lwt controller.byte

clean:
	camlbuild -clean

play:
	ocamlbuild -pkgs oUnit,yojson,str,lwt,cohttp.lwt,ANSITerminal main.byte && ./main.byte
