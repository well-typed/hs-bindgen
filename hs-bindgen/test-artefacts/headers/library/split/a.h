#ifndef SPLIT_A_H
#define SPLIT_A_H

// In one module the typedef 'foo' (Foo) makes 'struct foo' Foo_struct.
struct foo;
typedef struct foo *foo;

// Bar, with the field selector bar_x.
struct bar { int x; };

#endif
