default: libfeature-tests.so

.PHONY: clean
clean:
	rm -f *.o *.so

libfeature-tests.so: operators.o stream.o view.o
	gcc -Wall -shared -o libfeature-tests.so callbacks/operators.o callbacks/stream.o callbacks/view.o

# callbacks

operators.o: callbacks/operators.h callbacks/operators.c
	gcc -Wall -o callbacks/operators.o -c -fPIC callbacks/operators.c

stream.o: callbacks/stream.h callbacks/stream.c
	gcc -Wall -o callbacks/stream.o -c -fPIC callbacks/stream.c

view.o: callbacks/view.h callbacks/view.c
	gcc -Wall -o callbacks/view.o -c -fPIC callbacks/view.c
