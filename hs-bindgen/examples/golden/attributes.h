/**
 * Attributes
 *
 * See https://clang.llvm.org/docs/AttributeReference.html
 */

#define PACKED __attribute__((__packed__))

// named struct with attribute
struct __attribute__((__packed__)) foo {
    char c;
    int  i;
};

// named struct with macro attribute
struct PACKED bar {
    char c;
    int  i;
};

// anonymous struct with attribute
typedef struct __attribute__((__packed__)) {
    char c;
    int  i;
} baz;

// anonymous struct with macro attribute
typedef struct PACKED {
    char c;
    int  i;
} qux;

// Extract from _stdio.h on OSX
typedef struct __sFILE {
    int _r;
    int _w;
    int	(* _Nullable _close)(void *);
} FILE;
