
build:
	stack build

test: build
	$(MAKE) test -C test
	# TODO add a way to run the quickcheck tests

clean:
	stack clean
	$(MAKE) clean -C test

monitor:
	hdevtools admin -n --start-server
