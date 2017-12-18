PULP=/Users/acobb/.nvm/versions/node/v4.3.2/bin/pulp

default: distribution

build/project.js: src/Main.purs
	mkdir -p build
	$(PULP) browserify --optimise --to build/project.js

dist:
	mkdir -p dist

dist/min.js: build/project.js dist
	closure-compiler -O SIMPLE --js build/project.js --js_output_file dist/min.js

dist/index.html: index.html
	rm -f dist/index.html
	cp index.html dist/index.html

distribution: dist/min.js dist/index.html

deploy:
	scp dist/index.html dist/min.js "cirrus:/var/www/secure.cobbal.com/site/calendar-facts"

pulp:
	$(PULP) run

.PHONY: default run clean deploy pulp
