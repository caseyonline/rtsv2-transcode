.PHONY: all test compile

all: compile test

compile:
	spago build

test: | node_modules
	spago run # test --main Test

node_modules:
	npm install
