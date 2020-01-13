.PHONY: all

PUT = output

all:
	spago bundle-app --watch --main Main --to ./dist/rtsv2AppBundle.js & parcel --out-dir ../priv/www/assets/js/ --out-file rtsv2AppBundle.js ./dist/rtsv2AppBundle.js

clean:
	rm -rf node_modules output .spago dist/* *.lock
