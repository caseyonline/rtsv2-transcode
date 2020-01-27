.PHONY: all

PUT = output
BUNDLE_NAME = rtsv2AppBundle.js
PS_SOURCEFILE = ./dist/${BUNDLE_NAME}
JS_ASSET_FOLDER = ../priv/www/assets/js/

all: spago parcel

spago:
	spago bundle-app --main Main --to $(PS_SOURCEFILE)

parcel:
	parcel build --out-dir $(JS_ASSET_FOLDER) --out-file $(BUNDLE_NAME) $(PS_SOURCEFILE) --no-source-maps --no-cache

watch-spago:
	spago bundle-app --watch --main Main --to $(PS_SOURCEFILE)

watch-parcel:
	parcel watch --out-dir $(JS_ASSET_FOLDER) --out-file $(BUNDLE_NAME) $(PS_SOURCEFILE) --no-source-maps

clean-all:
	rm -rf node_modules output .spago .cache dist
