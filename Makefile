build:
	$(MAKE) -C client && $(MAKE) -C server

ser:
	$(MAKE) -C server

cli:
	$(MAKE) -C client

host:
	$(MAKE) -C server && ./server/serverhttp.byte

play:
	$(MAKE) -C client && ./client/main.byte

tester:
	ocamlbuild -pkgs yojson,cohttp.lwt,str -tag bin-annot -tag thread concurrent.byte

clean:
	ocamlbuild -clean
	$(CLEAN) -C client
