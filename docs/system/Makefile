DEFAULT := all

build/%.pdf: %.adoc ; asciidoctor-pdf -d book -o $@ $<

all: build/media-gateway.pdf

clean: ; rm -rf build/

.phony: default clean


