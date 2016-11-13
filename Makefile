test:
    ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal game_test.byte && ./game_test.byte

server:
    ocamlbuild server/* -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

client:
    ocamlbuild client/* -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

clean:
    ocamlbuild -clean
