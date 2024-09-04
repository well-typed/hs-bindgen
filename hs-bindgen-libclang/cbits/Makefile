# This Makefile is only used to able to check a bit more easily if the C code
# is correct or not. It is not required for the `hs-bindgen` build.

all: clang_wrappers.o doxygen_wrappers.o

clang_wrappers.o: clang_wrappers.c clang_wrappers.h
	gcc -c -Wall clang_wrappers.c -I/usr/lib/llvm-14/include

doxygen_wrappers.o: doxygen_wrappers.c doxygen_wrappers.h
	gcc -c -Wall doxygen_wrappers.c -I/usr/lib/llvm-14/include

.PHONY: clean
clean:
	rm -f *.o
