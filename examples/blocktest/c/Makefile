# `-lBlocksRuntime` must come AFTER iterator.o; some Linux linkers
# (Alpine/musl with --as-needed default) drop libraries that come before any
# object referencing their symbols, leading to undefined
# `_Block_copy`/`_NSConcreteStackBlock` etc. at link time.
libsimple_iterators.so: iterator.o
	clang -Wall -shared -o libsimple_iterators.so iterator.o -lBlocksRuntime

iterator.o:
	clang -Wall -std=c2x -fblocks -fPIC -c -o iterator.o iterator.c

.PHONY: clean
clean:
	rm -f *.o *.so
