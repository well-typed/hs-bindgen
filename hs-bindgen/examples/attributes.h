/**
 * Attributes
 *
 * See https://clang.llvm.org/docs/AttributeReference.html
 */

// Extract from _stdio.h on OSX
typedef struct __sFILE {
    int _r;
    int _w;
	int	(* _Nullable _close)(void *);
} FILE;