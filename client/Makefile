.PHONY: all clean

PS_SRC = src
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
OUTPUT = output

all: bundle.js

bundle.js: output
	purs bundle 'output/**/*.js' -o bundle.js -m 'Pure.Main' --main 'Pure.Main'
	cp bundle.js ../priv/www/assets/

#$(PS_SRC)/**/*.purs

output: $(PS_SOURCEFILES) .psc-package
	psc-package sources | xargs purs compile --codegen js  '$(PS_SRC)/**/*.purs'
	touch output

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)/*

