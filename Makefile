.PHONY: all server client test

all: server

server:
	$(MAKE) -C server all

client:
	$(MAKE) -C client all

test:
	$(MAKE) -C test all
