PROJECT_ROOT=../..
TARGET_EXT=.so

# There's a quirk with Apple assembler and LLVM IR that do not accept
# non-ASCII characters. So we only enable NON_ASCII characters by default.
#
# Check inside manual_examples.{c,h} for where this macro flag is used.
EXTRAFLAGS=-DNO_ASCII

# Simple OS detection. On Windows (via Git Bash, MinGW, etc.), OS is 'Windows_NT'.
ifeq ($(OS),Windows_NT)
	TARGET_EXT=.dll
	EXTRAFLAGS:=
endif

ifeq ($(shell uname -s) ,Linux)
	EXTRAFLAGS:=
endif

# If we are compiling with LLVM backend we can't use non-ASCII characters even
# if we are on Linux, so set this flag if intending to compile with LLVM
# backend.
ifeq ($(NO_ASCII),1)
	EXTRAFLAGS=-DNO_ASCII
endif

default: libexample${TARGET_EXT} libvector${TARGET_EXT} libgame${TARGET_EXT} libstructs${TARGET_EXT}

.PHONY: clean
clean:
	rm -f *.so *.dll *.o

# Basic examples

libexample${TARGET_EXT}: manual_examples.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libexample${TARGET_EXT} manual_examples.o

manual_examples.o: manual_examples.c ${PROJECT_ROOT}/hs-bindgen/examples/golden/manual_examples.h
	gcc ${EXTRAFLAGS} -Wall -o manual_examples.o -c -fPIC manual_examples.c -I${PROJECT_ROOT}/hs-bindgen/examples/golden

# Example for external bindings

libvector${TARGET_EXT}: vector.o vector_length.o vector_rotate.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libvector${TARGET_EXT} vector.o vector_length.o vector_rotate.o -lm

vector.o: vector.c vector.h
	gcc ${EXTRAFLAGS} -Wall -o vector.o -c -fPIC vector.c

vector_length.o: vector_length.c vector_length.h
	gcc ${EXTRAFLAGS} -Wall -o vector_length.o -c -fPIC vector_length.c

vector_rotate.o: vector_rotate.c vector_rotate.h
	gcc ${EXTRAFLAGS} -Wall -o vector_rotate.o -c -fPIC vector_rotate.c

libgame${TARGET_EXT}: game_world.o game_player.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libgame${TARGET_EXT} game_world.o game_player.o

game_world.o: game_world.c game_world.h
	gcc ${EXTRAFLAGS} -Wall -o game_world.o -c -fPIC game_world.c

game_player.o: game_player.c game_player.h
	gcc ${EXTRAFLAGS} -Wall -o game_player.o -c -fPIC game_player.c

libstructs${TARGET_EXT}: structs.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libstructs${TARGET_EXT} structs.o

structs.o: structs.c structs.h
	gcc ${EXTRAFLAGS} -Wall -o structs.o -c -fPIC structs.c
