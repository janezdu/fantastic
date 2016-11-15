build: 
	$(MAKE) -C client && $(MAKE) -C server

ser:
	$(MAKE) -C server

cli: 
	$(MAKE) -C client

host:
	$(MAKE) -C server && ./server/serverhttp.byte

play:
	$(MAKE) -C client && ./client/clienthttp.byte

clean:
	ocamlbuild -clean