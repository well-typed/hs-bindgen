.PHONY: all
all: libhs-bindgen-c-example.so hs-bindgen-c-example.pc

libhs-bindgen-c-example.so: hs-bindgen-c-example.o
	gcc -shared -o libhs-bindgen-c-example.so hs-bindgen-c-example.o

hs-bindgen-c-example.o: hs-bindgen-c-example.c
	gcc -c -Wall -Werror -fpic hs-bindgen-c-example.c

hs-bindgen-c-example.pc: hs-bindgen-c-example.pc-uninstalled
	echo "prefix=${PWD}" > hs-bindgen-c-example.pc
	cat hs-bindgen-c-example.pc-uninstalled >> hs-bindgen-c-example.pc

.PHONY: clean
clean:
	rm -f *.o *.so *.pc
