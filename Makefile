.PHONY: all server client test

all: client server

server:
	$(MAKE) -C server all

client:
	$(MAKE) -C client all

test:
	$(MAKE) -C test all
