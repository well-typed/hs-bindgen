// These macros are defined in the "wrong" order. We have to sort them before we
// can type check them.

// This simple test checks that we first parse macro types and expressions, sort
// them, and then type check macros later.
#define ValueB ValueA
#define ValueA 1

#define TypeB TypeA
#define TypeA int
