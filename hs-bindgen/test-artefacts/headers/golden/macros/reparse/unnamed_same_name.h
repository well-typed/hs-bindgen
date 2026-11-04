// The unnamed struct of field `b` is named `a_b`, like the named struct below.
// Each field `x` must keep its own type.
#define M int

struct a { struct { M x; } b; };
struct a_b { M *x; };
