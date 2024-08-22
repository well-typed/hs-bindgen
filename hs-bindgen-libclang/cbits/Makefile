# This Makefile is only used to able to check a bit more easily if the C code
# is correct or not. It is not required for the `hs-bindgen` build.

clang_wrappers.o: clang_wrappers.c clang_wrappers.h
	gcc -c -Wall clang_wrappers.c -I/usr/lib/llvm-18/include

.PHONY: clean
clean:
	rm -f *.o