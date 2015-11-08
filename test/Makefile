
BBCC=/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/bin/bbc
CC=gcc


test: aligned-bitfields aligned-bitfields.bb.h
	./aligned-bitfields

%.bb.h: %.bb
	${BBCC} -o $@ $<

%: %.bb.h
	${CC} -o $@ $@.c
