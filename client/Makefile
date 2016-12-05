build:
	ocamlbuild -pkgs yojson,lwt,cohttp.lwt,str,lambda-term simplerepl.byte

test:
	ocamlbuild -pkgs oUnit,core,yojson,lwt,cohttp.lwt,str,ANSITerminal client_test.byte && ./client_test.byte -runner sequential

simple:
	ocamlbuild -pkgs yojson,lwt,cohttp.lwt,str,lambda-term simplerepl.byte

clean:
	ocamlbuild -clean
