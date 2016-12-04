build:
	ocamlbuild -pkgs yojson,lwt,cohttp.lwt,str controller.byte && ocamlbuild -pkgs oUnit,yojson,str,lwt,cohttp.lwt,ANSITerminal main.byte

simple:
	ocamlbuild -pkgs yojson,lwt,cohttp.lwt,str,lambda-term simplerepl.byte


clean:
	ocamlbuild -clean
