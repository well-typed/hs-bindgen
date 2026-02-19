# Makefile for arch_types library
#
# This Makefile supports both native and cross-compilation builds.
#
# Usage:
#   make                          # Build for native platform
#   make CC=aarch64-linux-gnu-gcc # Cross-compile for aarch64
#   make clean                    # Remove build artifacts

CC ?= gcc
CFLAGS ?= -Wall -Wextra -fPIC -O2

# Library name
LIB_NAME = arch_types

# Source and object files
SRCS = arch_types.c
OBJS = $(SRCS:.c=.o)

# Output files
STATIC_LIB = lib$(LIB_NAME).a
SHARED_LIB = lib$(LIB_NAME).so

# Default target: build both static and shared libraries
all: $(STATIC_LIB) $(SHARED_LIB)

# Build static library
$(STATIC_LIB): $(OBJS)
	$(AR) rcs $@ $^

# Build shared library
$(SHARED_LIB): $(OBJS)
	$(CC) -shared -o $@ $^

# Compile source files
%.o: %.c arch_types.h
	$(CC) $(CFLAGS) -c $< -o $@

# Print architecture info (useful for debugging)
info:
	@echo "CC = $(CC)"
	@echo "Detected platform:"
	@$(CC) -dumpmachine 2>/dev/null || echo "(unable to detect)"
	@echo "sizeof(long) test:"
	@echo '#include <stdio.h>\nint main() { printf("%zu\\n", sizeof(long)); return 0; }' | \
		$(CC) -x c - -o /tmp/sizeof_test 2>/dev/null && /tmp/sizeof_test || echo "(cross-compilation - cannot run)"

clean:
	rm -f $(OBJS) $(STATIC_LIB) $(SHARED_LIB)

.PHONY: all clean info
