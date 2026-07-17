CC ?= clang
CFLAGS ?= -Wall -fPIC -O2 -I.

all: libfirstsecond.a

first.o: first.c first.h
	$(CC) $(CFLAGS) -c first.c

second.o: second.c second.h first.h
	$(CC) $(CFLAGS) -c second.c

libfirstsecond.a: first.o second.o
	$(AR) rcs $@ $^

clean:
	rm -f *.o *.a

.PHONY: all clean
