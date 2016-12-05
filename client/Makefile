
build:
	ocamlbuild -pkgs core,yojson,lwt,cohttp.lwt,str controller.byte && ocamlbuild -pkgs oUnit,core,yojson,str,lwt,cohttp.lwt,ANSITerminal main.byte

test: 
		ocamlbuild -pkgs oUnit,core,yojson,lwt,cohttp.lwt,str,ANSITerminal client_test.byte && ./client_test.byte -runner sequential

clean:
	ocamlbuild -clean
