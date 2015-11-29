
BBCC=$(shell stack exec which bbc)
CC=gcc

build: kafka

%.bb.h: %.bb
	${BBCC} -o $@ $<

kafka.o: kafka.c kafka-types.bb.h
	$(CC) $(FLAGS) $(CFLAGS) -c $<

kafka: kafka.o
	${CC} -o kafka kafka.o

clean:
	rm -f *.bb.h
	rm -f kafka
