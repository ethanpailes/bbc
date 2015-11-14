
build:
	stack build

test: build
	stack exec bbc -- --test
	$(MAKE) test -C test

clean:
	stack clean
	$(MAKE) clean -C test

