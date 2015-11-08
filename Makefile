
build:
	stack build

test: build
	stack exec bbc -- -test

clean:
	stack clean

