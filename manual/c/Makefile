PROJECT_ROOT=../..

libexample.so: manual_examples.o
	gcc -o libexample.so -shared manual_examples.o

manual_examples.o: manual_examples.c ${PROJECT_ROOT}/hs-bindgen/examples/manual_examples.h
	gcc -o manual_examples.o -c -fPIC manual_examples.c -I${PROJECT_ROOT}/hs-bindgen/examples

.PHONY: clean
clean:
	rm -f *.so *.o
