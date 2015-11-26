
BBCC=$(shell stack exec which bbc)
CC=gcc

test: clean \
			aligned-bitfields          aligned-bitfields.bb.h \
			named-block                named-block.bb.h \
			array                      array.bb.h \
			array-and-nested           array-and-nested.bb.h \
			large-array                array.bb.h
	./test.sh aligned-bitfields 0 ./outfiles/aligned-bitfields.out aligned-bitfields
	./test.sh compile-unknown-block.sh 1 ./outfiles/compile-unknown-block.sh.out unknown-block
	./test.sh named-block 0 ./outfiles/named-block.out named-block
	./test.sh array 0 ./outfiles/array.out array
	./test.sh array-and-nested 0 ./outfiles/array-and-nested.out array-and-nested
	./test.sh large-array 0 ./outfiles/large-array.out large-array

%.bb.h: %.bb
	${BBCC} -o $@ $<

%: %.bb.h
	${CC} -o $@ $@.c

clean:
	rm -f *.bb.h
	rm -f aligned-bitfields named-block unknown-block
