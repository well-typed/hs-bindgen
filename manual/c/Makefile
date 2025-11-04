PROJECT_ROOT=../..
TARGET_EXT=.so


# There's a quirk with Apple and Windows assembler and LLVM IR that do not
# accept Unicode characters. There's SUPPORTS_UNICODE flag that allows Unicode
# characters and we only enable that for non-MacOS and non-LLVM backend
# compilation
#
# Check inside manual_examples.{c,h} for where this macro flag is used.
#
EXTRAFLAGS:=

ifeq ($(shell uname -s),Linux)
	EXTRAFLAGS:=-DSUPPORTS_UNICODE
endif

# On Windows we have to rename the output files to .dll extension
ifeq ($(OS),Windows_NT)
	TARGET_EXT=.dll
endif

# If we are compiling with LLVM backend we can't use Unicode characters even
# if we are on Linux, so if intending to compile with LLVM backend (i.e.
# LLVM_BACKEND is set), we unset the SUPPORTS_UNICODE flag
ifeq ($(LLVM_BACKEND),1)
	EXTRAFLAGS:=
endif

default: libexample${TARGET_EXT} libvector${TARGET_EXT} libgame${TARGET_EXT} libstructs${TARGET_EXT} libglobals${TARGET_EXT} libarrays${TARGET_EXT} libfunctionpointers${TARGET_EXT} libhsb_complex_test${TARGET_EXT} libcallbacks${TARGET_EXT} libzerocopy${TARGET_EXT}

.PHONY: clean
clean:
	rm -f *.so *.dll *.o

# Basic examples

libexample${TARGET_EXT}: manual_examples.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libexample${TARGET_EXT} manual_examples.o

manual_examples.o: manual_examples.c manual_examples.h
	gcc ${EXTRAFLAGS} -Wall -o manual_examples.o -c -fPIC manual_examples.c -I${PROJECT_ROOT}/hs-bindgen/manual/c

libstructs${TARGET_EXT}: structs.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libstructs${TARGET_EXT} structs.o

structs.o: structs.c structs.h
	gcc ${EXTRAFLAGS} -Wall -o structs.o -c -fPIC structs.c

libglobals${TARGET_EXT}: globals.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libglobals${TARGET_EXT} globals.o

globals.o: globals.c globals.h
	gcc ${EXTRAFLAGS} -Wall -o globals.o -c -fPIC globals.c

libarrays${TARGET_EXT}: arrays.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libarrays${TARGET_EXT} arrays.o

arrays.o: arrays.c arrays.h
	gcc ${EXTRAFLAGS} -Wall -o arrays.o -c -fPIC arrays.c

libfunctionpointers${TARGET_EXT}: function_pointers.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libfunctionpointers${TARGET_EXT} function_pointers.o

function_pointers.o: function_pointers.c function_pointers.h
	gcc ${EXTRAFLAGS} -Wall -o function_pointers.o -c -fPIC function_pointers.c

libhsb_complex_test${TARGET_EXT}: hsb_complex_test.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libhsb_complex_test${TARGET_EXT} hsb_complex_test.o

hsb_complex_test.o: hsb_complex_test.c hsb_complex_test.h
	gcc ${EXTRAFLAGS} -Wall -o hsb_complex_test.o -c -fPIC hsb_complex_test.c

libcallbacks${TARGET_EXT}: callbacks.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libcallbacks${TARGET_EXT} callbacks.o

callbacks.o: callbacks.c callbacks.h
	gcc ${EXTRAFLAGS} -Wall -o callbacks.o -c -fPIC callbacks.c

libzerocopy${TARGET_EXT}: zero_copy.o
	gcc ${EXTRAFLAGS} -Wall -shared -o libzerocopy${TARGET_EXT} zero_copy.o

zero_copy.o: zero_copy.c zero_copy.h
	gcc ${EXTRAFLAGS} -Wall -o zero_copy.o -c -fPIC zero_copy.c

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
