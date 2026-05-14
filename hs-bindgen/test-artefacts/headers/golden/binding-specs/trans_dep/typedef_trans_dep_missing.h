// Unselected
typedef int A;

// Replaced by external binding specifications
typedef A B;

// foo :: Ptr B -> ... or foo :: FunPtr B -> ... ?
// (depends on B = A = int, therefore Ptr)
void foo(B *x);
