build: 
	$(MAKE) -C client && $(MAKE) -C server

ser:
	$(MAKE) -C server

cli: 
	$(MAKE) -C client

clean:
	ocamlbuild -clean