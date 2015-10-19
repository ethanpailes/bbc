

test:
	stack clean
	stack build && stack exec bbc -- -test
