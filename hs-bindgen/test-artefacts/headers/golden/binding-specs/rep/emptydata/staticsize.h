#include "staticsize_deps.h"

// Every type below is given the emptydata representation in staticsize_p.yaml.
// A StaticSize instance is generated whenever the underlying C type has a known
// size and alignment.

// --- Complete C types: struct, union, and enum carry their layout directly. ---

struct point2d { int x; int y; };

union value { int i; double d; };

enum color { RED, GREEN, BLUE };

// --- Typedefs: resolved to their underlying complete type. ---

typedef struct pt   td_struct;   // typedef -> struct
typedef pt_alias    td_chain;    // typedef -> typedef -> struct
typedef union shape td_union;    // typedef -> union

// --- No size available at this pass: no StaticSize instance is generated. ---

typedef int counter;             // typedef -> primitive

struct opaque_fwd;               // opaque (forward declaration)

#define macro_ptr int *          // macro -> pointer
