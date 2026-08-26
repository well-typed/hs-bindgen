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

# If we are compiling with LLVM backend we can't use Unicode characters even
# if we are on Linux, so if intending to compile with LLVM backend (i.e.
# LLVM_BACKEND is set), we unset the SUPPORTS_UNICODE flag
ifeq ($(LLVM_BACKEND),1)
	EXTRAFLAGS:=
endif

# On Windows we have to rename the output files to .dll extension
ifeq ($(OS),Windows_NT)
	TARGET_EXT=.dll
endif

default: \
	libmanual${TARGET_EXT} \
	libvector${TARGET_EXT} \
	libgame${TARGET_EXT} \

.PHONY: clean
clean:
	rm -f *.so *.dll *.o */*.o

# Manual

libmanual${TARGET_EXT}: \
		manual_examples.o \
		macro.o \
		globals.o \
		arrays.o \
		function_pointers.o \
		hsb_complex_test.o \
		callbacks.o \
		generated_names.o \
		generated-names/unprefixed_field_names.o \
		pointer_manipulation.o \
		record_dot_syntax.o \
		structs.o \
		structs/nesting.o \
		unions.o \
		unions/nesting.o

	clang ${EXTRAFLAGS} -Wall -shared -o libmanual${TARGET_EXT} \
		manual_examples.o \
		macro.o \
		globals.o \
		arrays.o \
		function_pointers.o \
		hsb_complex_test.o \
		callbacks.o \
		generated_names.o \
		generated-names/unprefixed_field_names.o \
		pointer_manipulation.o \
		record_dot_syntax.o \
		structs.o \
		structs/nesting.o \
		unions.o \
		unions/nesting.o

manual_examples.o: manual_examples.c manual_examples.h
	clang ${EXTRAFLAGS} -Wall -o manual_examples.o -c -fPIC manual_examples.c -I${PROJECT_ROOT}/hs-bindgen/manual/c

macro.o: macro.c macro.h
	clang ${EXTRAFLAGS} -Wall -o macro.o -c -fPIC macro.c

globals.o: globals.c globals.h
	clang ${EXTRAFLAGS} -Wall -o globals.o -c -fPIC globals.c

arrays.o: arrays.c arrays.h
	clang ${EXTRAFLAGS} -Wall -o arrays.o -c -fPIC arrays.c

function_pointers.o: function_pointers.c function_pointers.h
	clang ${EXTRAFLAGS} -Wall -o function_pointers.o -c -fPIC function_pointers.c

hsb_complex_test.o: hsb_complex_test.c hsb_complex_test.h
	clang ${EXTRAFLAGS} -Wall -o hsb_complex_test.o -c -fPIC hsb_complex_test.c

callbacks.o: callbacks.c callbacks.h
	clang ${EXTRAFLAGS} -Wall -o callbacks.o -c -fPIC callbacks.c

## Generated names

generated_names.o: generated_names.c generated_names.h
	clang ${EXTRAFLAGS} -Wall -o generated_names.o -c -fPIC generated_names.c

generated-names/unprefixed_field_names.o: generated-names/unprefixed_field_names.c generated-names/unprefixed_field_names.h
	clang ${EXTRAFLAGS} -Wall -o generated-names/unprefixed_field_names.o -c -fPIC generated-names/unprefixed_field_names.c

## Pointer manipulation API

pointer_manipulation.o: pointer_manipulation.c pointer_manipulation.h
	clang ${EXTRAFLAGS} -Wall -o pointer_manipulation.o -c -fPIC pointer_manipulation.c

## Record dot syntax

record_dot_syntax.o: record_dot_syntax.c record_dot_syntax.h
	clang ${EXTRAFLAGS} -Wall -o record_dot_syntax.o -c -fPIC record_dot_syntax.c

## Structs

structs.o: structs.c structs.h
	clang ${EXTRAFLAGS} -Wall -o structs.o -c -fPIC structs.c

structs/nesting.o: structs/nesting.c structs/nesting.h
	clang ${EXTRAFLAGS} -Wall -Wno-missing-declarations -o structs/nesting.o -c -fPIC structs/nesting.c

## Unions

unions.o: unions.c unions.h
	clang ${EXTRAFLAGS} -Wall -o unions.o -c -fPIC unions.c

unions/nesting.o: unions/nesting.c unions/nesting.h
	clang ${EXTRAFLAGS} -Wall -Wno-missing-declarations -o unions/nesting.o -c -fPIC unions/nesting.c



# External bindings: vector example

libvector${TARGET_EXT}: vector.o vector_length.o vector_rotate.o
	clang ${EXTRAFLAGS} -Wall -shared -o libvector${TARGET_EXT} vector.o vector_length.o vector_rotate.o -lm

vector.o: vector.c vector.h
	clang ${EXTRAFLAGS} -Wall -o vector.o -c -fPIC vector.c

vector_length.o: vector_length.c vector_length.h
	clang ${EXTRAFLAGS} -Wall -o vector_length.o -c -fPIC vector_length.c

vector_rotate.o: vector_rotate.c vector_rotate.h
	clang ${EXTRAFLAGS} -Wall -o vector_rotate.o -c -fPIC vector_rotate.c

# External bindings: vector example

libgame${TARGET_EXT}: game_world.o game_player.o
	clang ${EXTRAFLAGS} -Wall -shared -o libgame${TARGET_EXT} game_world.o game_player.o

game_world.o: game_world.c game_world.h
	clang ${EXTRAFLAGS} -Wall -o game_world.o -c -fPIC game_world.c

game_player.o: game_player.c game_player.h
	clang ${EXTRAFLAGS} -Wall -o game_player.o -c -fPIC game_player.c
