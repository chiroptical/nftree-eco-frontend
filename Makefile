build:
	spago build

test:
	spago test

serve:
	parcel dev/index.html --port 3000

watch:
	spago bundle-app --watch --to dev/index.js

build-prod:
	mkdir -p prod
	cp dev/index.html prod
	rm -rf dist
	spago bundle-app --to prod/index.js
	parcel build prod/index.html

format:
	purty format src/ --write

.PHONY: build test serve watch build-prod format
