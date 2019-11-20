.PHONY: all clean repl

default: all

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)
COMPILED_PS = ../src/compiled_ps
ERL_MODULES_VERSION = $(shell dhall-to-json < packages.dhall | jq '."erl-modules".version')

PACKAGE_SET = $(shell jq '.set' < psc-package.json)
ERL_MODULES_VERSION = $(shell jq '."erl-modules".version' < .psc-package/$(PACKAGE_SET)/.set/packages.json)

all: $(COMPILED_PS)

$(COMPILED_PS): output
	mkdir -p $(COMPILED_PS)
	cp -r output/* $(COMPILED_PS)
	@touch $(COMPILED_PS)

#output: $(PS_SOURCEFILES) $(PS_ERL_FFI) .spago
#	.spago/erl-modules/${ERL_MODULES_VERSION}/scripts/gen_module_names.sh src/
#	spago build
#	@touch output
#
#.spago: spago.dhall packages.dhall
#	spago install
#	touch .spago

output: $(PS_SOURCEFILES) $(PS_ERL_FFI) .psc-package
	psc-package sources | xargs purs compile '$(PS_SRC)/**/*.purs'
	@touch output

repl: $(PS_SOURCEFILES) $(PS_ERL_FFI) .psc-package
	@echo xargs purs repl '$(PS_SRC)/**/*.purs'

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)
