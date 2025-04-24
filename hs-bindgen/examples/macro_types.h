// Ptr CInt
#define PtrInt int*

// Ptr (Ptr CChar)
#define PtrPtrChar char**

// ConstantArray 2 CInt
#define Arr1 int[2]

// ConstantArray 3 (Ptr Float)
#define Arr2 float *[3]

// Array 4 ( FunPtr ( Double -> Float ) )
#define Arr3 float (*[4])(double)

// CInt -> Ptr CFloat
#define Fun1 float *(int)

// FunPtr (CFloat -> Ptr CDouble -> CInt)
#define Fun2 int (*)(float, double *)

// FunPtr (Ptr CFloat -> Ptr CInt)
#define Fun3 int* (*)(float *)

// CInt -> Ptr CLong -> FunPtr ( CFloat -> Ptr CDouble -> Ptr CLong )
#define Fun4 long * (*(int, long *))(float, double *)

// ConstantArray 8 CChar -> Ptr ( ConstantArray 2 ( Ptr CShort ) )
#define Fun5 short *(* (char[8]))[2]


// Reparsing typedefs
#define MTy float
typedef MTy tty;

#define UINT8_T unsigned char
#define BOOLEAN_T UINT8_T
typedef BOOLEAN_T boolean_T;
