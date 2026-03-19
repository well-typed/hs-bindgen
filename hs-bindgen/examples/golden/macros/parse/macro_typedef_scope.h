// Check that the appropriate typedefs are in scope as we parse macros. This
// depends on processing macros in source order (rather than all at the start).

#define M1 int
typedef M1 T2;
#define M3 T2
typedef M3 T4;
