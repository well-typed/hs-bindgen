libsimple_iterators.so: iterator.o
	clang -Wall -shared -lBlocksRuntime -o libsimple_iterators.so iterator.o

iterator.o:
	clang -Wall -std=c2x -fblocks -fPIC -c -o iterator.o iterator.c

.PHONY: clean
clean:
	rm -f *.o *.so
