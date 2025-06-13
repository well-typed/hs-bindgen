PROJECT_ROOT=../..

default: libexample.so libvector.so libgame.so

.PHONY: clean
clean:
	rm -f *.so *.o

# Basic examples

libexample.so: manual_examples.o
	gcc -Wall -o libexample.so -shared manual_examples.o

manual_examples.o: manual_examples.c ${PROJECT_ROOT}/hs-bindgen/examples/golden/manual_examples.h
	gcc -Wall -o manual_examples.o -c -fPIC manual_examples.c -I${PROJECT_ROOT}/hs-bindgen/examples/golden

# Example for external bindings

libvector.so: vector.o vector_length.o vector_rotate.o
	gcc -Wall -o libvector.so -shared vector.o vector_length.o vector_rotate.o -lm

vector.o: vector.c vector.h
	gcc -Wall -o vector.o -c -fPIC vector.c

vector_length.o: vector_length.c vector_length.h
	gcc -Wall -o vector_length.o -c -fPIC vector_length.c

vector_rotate.o: vector_rotate.c vector_rotate.h
	gcc -Wall -o vector_rotate.o -c -fPIC vector_rotate.c

libgame.so: game_world.o game_player.o
	gcc -Wall -o libgame.so -shared game_world.o game_player.o

game_world.o: game_world.c game_world.h
	gcc -Wall -o game_world.o -c -fPIC game_world.c

game_player.o: game_player.c game_player.h
	gcc -Wall -o game_player.o -c -fPIC game_player.c


