libblocktest: iterator.o
	clang -Wall -shared -lBlocksRuntime -o libblocktest.so iterator.o

iterator.o:
	clang -Wall -std=c23 -fblocks -fPIC -c -o iterator.o iterator.c

.PHONY: clean
clean:
	rm -f *.o *.so
