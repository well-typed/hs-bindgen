// A comma in a macro body denotes a tuple, not the C comma operator.
//
// C assigns no meaning to macro bodies, so we are free to pick the most useful
// interpretation. Tuples support the argument-list idiom (`#define ARGS 1, 2`
// used as `f(ARGS)`); the comma operator is pointless here, since the
// expressions we can express have no side effects. See issue 2182.

#define OBJ            (1, 2)
#define OBJ_NO_PARENS  1, 2
#define FUN(x, y)      (x, y)
#define FUN_THREE(x, y, z) ((x), (y), (z))

// Consequence of the tuple interpretation: a comma expression does not compose
// with arithmetic, so this macro fails to parse.
#define ARITH          ((1, 2) + 3)
