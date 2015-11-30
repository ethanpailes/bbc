
BBCC=$(shell stack exec which bbc)
CC=gcc
CFLAGS=

build: kafka

%.bb.h: %.bb
	${BBCC} -o $@ $<

kafka.o: kafka.c kafka-types.bb.h
	$(CC) $(FLAGS) $(CFLAGS) -c $<

kafka: kafka.o kafka-types.bb
	${CC} ${FLAGS} ${CFLAGS} -o kafka kafka.o

clean:
	rm -f *.bb.h
	rm -f kafka
