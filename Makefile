build:
	spago build

test:
	spago test

serve:
	parcel dev/index.html --port 3000

watch:
	spago bundle-app --watch --to dev/index.js

build-prod: tailwind-css
	mkdir -p prod
	cp dev/index.html prod
	spago bundle-app --to prod/index.js
	parcel build prod/index.html --no-minify

format:
	purty format src/ --write

# The tailwind commands are meant to be run in sequential order
tailwind-base-css:
	npx tailwindcss -o dev/tailwind.css

tailwind-classes:
	twpurs gen-available-classes

tailwind-purs:
	twpurs gen-purs

tailwind-css:
	twpurs gen-css --css dev/tailwind.css --out prod/tailwind.css

.PHONY: build test serve watch build-prod format tailwind-base-css tailwind-classes tailwind-purs tailwind-css
