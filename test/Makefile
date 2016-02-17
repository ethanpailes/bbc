
BBCC=$(shell stack exec which bbc)
CC=gcc

test: clean \
	  aligned-bitfields.bb.h \
	  aligned-bitfields \
	  named-block.bb.h \
	  named-block \
	  array.bb.h \
	  array \
	  array-and-nested.bb.h \
	  array-and-nested \
	  large-array \
	  sumty.bb.h \
	  sumty \
	  sumty-and-nested.bb.h \
	  sumty-and-nested \
	  bad-read.bb.h \
	  bad-read \
	  fixed-array.bb.h \
	  fixed-array
	./test.sh aligned-bitfields 0 ./outfiles/aligned-bitfields.out aligned-bitfields
	./test.sh compile-unknown-block.sh 1 ./outfiles/compile-unknown-block.sh.out unknown-block
	./test.sh named-block 0 ./outfiles/named-block.out named-block
	./test.sh array 0 ./outfiles/array.out array
	./test.sh array-and-nested 0 ./outfiles/array-and-nested.out array-and-nested
	./test.sh large-array 0 ./outfiles/large-array.out large-array
	./test.sh sumty 0 ./outfiles/sumty.out sumty
	./test.sh sumty-and-nested 0 ./outfiles/sumty-and-nested.out sumty-and-nested
	./test.sh bad-read 0 ./outfiles/bad-read.out bad-read
	./test.sh fixed-array 0 ./outfiles/fixed-array.out fixed-array


%.bb.h: %.bb
	${BBCC} -o $@ $<

%: %.bb.h
	${CC} -o $@ $@.c

clean:
	rm -f *.bb.h
	rm -f \
	  aligned-bitfields.bb.h \
	  aligned-bitfields \
	  named-block.bb.h \
	  named-block \
	  array.bb.h \
	  array \
	  array-and-nested.bb.h \
	  array-and-nested \
	  large-array \
	  sumty.bb.h \
	  sumty \
	  sumty-and-nested.bb.h \
	  sumty-and-nested \
	  bad-read.bb.h \
	  bad-read \
	  fixed-array.bb.h \
	  fixed-array
