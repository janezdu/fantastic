
build:
	ocamlbuild -pkgs core,yojson,lwt,cohttp.lwt,str controller.byte && ocamlbuild -pkgs oUnit,core,yojson,str,lwt,cohttp.lwt,ANSITerminal main.byte

test:
	ocamlbuild -pkgs oUnit,core,yojson,str,lwt,cohttp.lwt,ANSITerminal,unix client_test.byte && ./client_test.byte


clean:
	ocamlbuild -clean
