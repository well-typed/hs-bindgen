# Makefile for simple C library

CC = gcc
CFLAGS = -Wall -Wextra -fPIC -shared
LDFLAGS = -shared

# Detect OS for library naming
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    LIB_EXT = so
    LIB_NAME = libc-lib.so
endif
ifeq ($(UNAME_S),Darwin)
    LIB_EXT = dylib
    LIB_NAME = libc-lib.dylib
    LDFLAGS += -dynamiclib -install_name @rpath/$(LIB_NAME)
endif
ifeq ($(OS),Windows_NT)
    LIB_EXT = dll
    LIB_NAME = c-lib.dll
endif

# Source and object files
SRCS = simple.c
OBJS = $(SRCS:.c=.o)

# Default target
all: $(LIB_NAME)

# Build shared library
$(LIB_NAME): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

# Compile source files
%.o: %.c simple.h
	$(CC) $(CFLAGS) -c $< -o $@

# Clean build artifacts
clean:
	rm -f $(OBJS) $(LIB_NAME) *.so *.dylib *.dll

.PHONY: all clean install
