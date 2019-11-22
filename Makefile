.PHONY: all server client test

all: server client test

server:
	$(MAKE) -C server all

client:
	$(MAKE) -C client all

test:
	$(MAKE) -C test all
