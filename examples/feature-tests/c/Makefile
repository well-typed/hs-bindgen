default: libfeature-tests.so

CC = clang

.PHONY: clean
clean:
	rm -f *.o *.so

libfeature-tests.so: \
		arrays/known_size.o \
		arrays/unknown_size.o \
		basic.o \
		structs.o \
		unions.o \
		types/anonymous.o \
		types/bitfields.o

	$(CC) -Wall -shared -o libfeature-tests.so \
		callbacks/arrays/known_size.o \
		callbacks/arrays/unknown_size.o \
		callbacks/basic.o \
		callbacks/structs.o \
		callbacks/unions.o \
		types/anonymous.o \
		types/bitfields.o

# callbacks

arrays/known_size.o: callbacks/arrays/known_size.h callbacks/arrays/known_size.c
	$(CC) -Wall -o callbacks/arrays/known_size.o -c -fPIC callbacks/arrays/known_size.c

arrays/unknown_size.o: callbacks/arrays/unknown_size.h callbacks/arrays/unknown_size.c
	$(CC) -Wall -o callbacks/arrays/unknown_size.o -c -fPIC callbacks/arrays/unknown_size.c

basic.o: callbacks/basic.h callbacks/basic.c
	$(CC) -Wall -o callbacks/basic.o -c -fPIC callbacks/basic.c

structs.o: callbacks/structs.h callbacks/structs.c
	$(CC) -Wall -o callbacks/structs.o -c -fPIC callbacks/structs.c

unions.o: callbacks/unions.h callbacks/unions.c
	$(CC) -Wall -o callbacks/unions.o -c -fPIC callbacks/unions.c

# types

## anonymous

types/anonymous.o: types/anonymous.h types/anonymous.c
	$(CC) -Wall -o types/anonymous.o -c -fPIC types/anonymous.c

## bitfields

types/bitfields.o: types/bitfields.h types/bitfields.c
	$(CC) -Wall -o types/bitfields.o -c -fPIC types/bitfields.c
