default: lib

.PHONY: clean
clean:
	rm -f *.so *.dll *.o main

lib: foo.o main.o
	gcc -Wall -o main foo.o main.o

foo.o:
	gcc -Wall -o foo.o -c -fPIC foo.c

main.o:
	gcc -Wall -o main.o -c -fPIC main.c
