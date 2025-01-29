libexample.so: no_external_headers.o
	gcc -o libexample.so -shared no_external_headers.o

no_external_headers.o: no_external_headers.c no_external_headers.h
	gcc -o no_external_headers.o -c -fPIC no_external_headers.c

.PHONY: clean
clean:
	rm -f *.so *.o