
#
# For some reason nixos can't find which under stack exec, so we have to
# first find the absolute path of which, then use that to find bbc
WHICH=$(shell which which)
BBCC=$(shell stack exec ${WHICH} bbc)
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
	  fixed-array \
	  fixed-array-nested.bb.h \
	  fixed-array-nested \
	  fixed-array-sumty.bb.h \
	  fixed-array-sumty
	./test.sh aligned-bitfields 0 \
					./outfiles/aligned-bitfields.out aligned-bitfields
	./test.sh compile-unknown-block.sh 1 \
					./outfiles/compile-unknown-block.sh.out unknown-block
	./test.sh named-block 0 ./outfiles/named-block.out named-block
	./test.sh array 0 ./outfiles/array.out array
	./test.sh array-and-nested 0 ./outfiles/array-and-nested.out array-and-nested
	./test.sh large-array 0 ./outfiles/large-array.out large-array
	./test.sh sumty 0 ./outfiles/sumty.out sumty
	./test.sh sumty-and-nested 0 ./outfiles/sumty-and-nested.out sumty-and-nested
	./test.sh bad-read 0 ./outfiles/bad-read.out bad-read
	./test.sh fixed-array 0 ./outfiles/fixed-array.out fixed-array
	./test.sh fixed-array-nested 0 \
					./outfiles/fixed-array-nested.out fixed-array-nested
	./test.sh fixed-array-sumty 0 \
					./outfiles/fixed-array-sumty.out fixed-array-sumty
	./test.sh compile-bad-fixed-array.sh 1 \
					./outfiles/compile-bad-fixed-array.sh.out compile-bad-fixed-array.sh


%.bb.h: %.bb
	${BBCC} -n -o $@ $<

%: %.bb.h
	${CC} -o $@ $@.c

clean:
	# rm -f *.bb.h
    # aligned-bitfields.bb.h 
	rm -f \
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
	  fixed-array \
	  fixed-array-nested.bb.h \
	  fixed-array-nested \
	  fixed-array-sumty.bb.h \
	  fixed-array-sumty \
	  fixed-array-type-error
