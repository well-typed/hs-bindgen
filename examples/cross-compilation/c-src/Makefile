CC ?= clang
CFLAGS ?= -Wall -fPIC -O2

all: libarch_types.a libarch_types.so

arch_types.o: arch_types.c arch_types.h
	$(CC) $(CFLAGS) -c arch_types.c

libarch_types.a: arch_types.o
	$(AR) rcs $@ $^

libarch_types.so: arch_types.o
	$(CC) $(CFLAGS) -shared -o $@ $^

clean:
	rm -f arch_types.o libarch_types.a libarch_types.so

.PHONY: all clean
