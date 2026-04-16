// Pointer-to-void is a valid macro type (even though bare void is not)
#define PtrToVoid void *

// const void * is also valid (pointer to const void)
#define PtrToConstVoidL const void *

// void const * is also valid (pointer to const void)
#define PtrToConstVoidR void const *

// Pointer to const int (const qualifies the pointee)
#define PtrToConstIntL const int *

// Pointer to const int (const qualifies the pointee)
#define PtrToConstIntR int const *

// Const pointer to int (const qualifies the pointer itself)
#define ConstPtrToInt int *const
