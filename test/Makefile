
BBCC=/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/bin/bbc
CC=gcc


test: aligned-bitfields aligned-bitfields.bb.h
	./test.sh aligned-bitfields 0 ./outfiles/aligned-bitfields.out aligned-bitfields
	./test.sh compile-unknown-block.sh 1 ./outfiles/compile-unknown-block.sh.out unknown-block

%.bb.h: %.bb
	${BBCC} -o $@ $<

%: %.bb.h
	${CC} -o $@ $@.c
