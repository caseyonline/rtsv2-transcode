.PHONY: all test compile clean

all: compile test

install:
	npm install
	spago install

compile:
	spago build

test:
	spago test --main Test
