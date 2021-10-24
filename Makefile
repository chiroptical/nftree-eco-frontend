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
	twpurs gen-css --src src/Page --css prod/tailwind.css
	spago bundle-app --to prod/index.js
	parcel build prod/index.html

format:
	purty format src/ --write

# The tailwind commands are meant to be run in sequential order
tailwind-css:
	npx tailwindcss -o dev/tailwind.css

tailwind-classes:
	twpurs gen-available-classes

tailwind-purs:
	twpurs gen-purs

.PHONY: build test serve watch build-prod format tailwind-css tailwind-classes tailwind-purs
