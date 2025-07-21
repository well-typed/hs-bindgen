default: lib

.PHONY: clean
clean:
	rm -f *.so *.dll *.o main

lib: libfoo.so main.o
	gcc -Wall -o main main.o libfoo.so

foo.o:
	gcc -Wall -o foo.o -c -fPIC foo.c

libfoo.so: foo.o
	gcc -Wall --shared -o libfoo.so foo.o

main.o:
	gcc -Wall -o main.o -c -fPIC main.c
