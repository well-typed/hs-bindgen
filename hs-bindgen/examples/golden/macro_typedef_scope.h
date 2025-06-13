// Check that the appropriate typedefs are in scope as we parse macros. This
// depends on processing macros in source order (rather than all at the start).

#define T1 int  // T1 = int
typedef T1 T2;  // T2 = T1
#define T3 T2   // T3 = T2
typedef T3 T4;  // T4 = T3
