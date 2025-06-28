/**
 * Examples from the c2hsc test suite
 *
 * https://github.com/jwiegley/c2hsc/blob/master/test/main.hs
 */

// #38: typedef const is seen as const rather than typedef
typedef const char* an_pchar;

// #25: incorrect code generated for char x[8][255]
typedef struct {
    char listOfNames[8][255];
} MyCoolStruct;

// #17: function pointer types
typedef int (*foo)(int);

// #17: function pointer arrays
//
// NOTE: Added the `extern` keyword; it is required to declare arrays with
// unspecified size.
//
// TODO: We don't generate anything for this yet (global variable)
extern int (*my_array[])(int);

//


// --             it "" $
// --                 matches [here|
// -- |] [here|
// -- #callback my_array_callback , CInt -> IO CInt
// -- #globalvar my_array , <my_array_callback>
// -- |]

// --             it "function pointer structure members" $
// --                 matches [here|
// -- struct foo_t {
// --     int (*foo_member)(int);
// -- };
// -- |] [here|
// -- {- struct foo_t {
// --     int (* foo_member)(int);
// -- }; -}
// -- #callback foo_member_callback , CInt -> IO CInt
// -- #starttype struct foo_t
// -- #field foo_member , <foo_member_callback>
// -- #stoptype
// -- |]

// --             it "function pointer function arguments" $
// --                 matches [here|
// -- void foo_function(int (*foo)(int)) {}
// -- |] [here|
// -- #callback foo_function_foo_callback , CInt -> IO CInt
// -- #cinline foo_function , <foo_function_foo_callback> -> IO ()
// -- #include <bindings.cmacros.h>

// -- BC_INLINE1VOID(foo_function, int, int)
// -- |]

//         it "#15" $
//             matches [here|
// typedef struct Foo_ Foo;
// typedef enum Bar_ { BAR } Bar;
// |] [here|
// {- typedef struct Foo_ Foo; -}
// #opaque_t struct Foo_
// #synonym_t Foo , <struct Foo_>
// {- typedef enum Bar_ {
//             BAR
//         } Bar; -}
// #integral_t enum Bar_
// #num BAR
// #synonym_t Bar , <enum Bar_>
// |]

//         it "#12" $
//             matches [here|
// struct st {
//   int i;
// };

// enum e {
//   CONST
// };

// union u {
//   char c;
// };
// |] [here|
// {- struct st {
//     int i;
// }; -}
// #starttype struct st
// #field i , CInt
// #stoptype
// {- enum e {
//     CONST
// }; -}
// #integral_t enum e
// #num CONST
// {- union u {
//     char c;
// }; -}
// #starttype union u
// #field c , CChar
// #stoptype
// |]
//         it "#15" $
//             matches [here|
// struct MyTypeImpl;
// typedef struct MyTypeImpl* MyType;

// typedef struct MyStruct {
//   int x;
// } MyStructType;

// typedef struct MyStructEmpty MyStructEmptyType;
// |] [here|
// {- struct MyTypeImpl; -}
// #opaque_t struct MyTypeImpl
// {- typedef struct MyTypeImpl * MyType; -}
// #synonym_t MyType , Ptr <struct MyTypeImpl>
// {- typedef struct MyStruct {
//             int x;
//         } MyStructType; -}
// #starttype struct MyStruct
// #field x , CInt
// #stoptype
// #synonym_t MyStructType , <struct MyStruct>
// {- typedef struct MyStructEmpty MyStructEmptyType; -}
// #opaque_t struct MyStructEmpty
// #synonym_t MyStructEmptyType , <struct MyStructEmpty>
// |]

//         it "#33" $ do
//             matches [here|
// unsigned foo(unsigned);
// |] [here|
// #ccall foo , CUInt -> IO CUInt
// |]
//             matches [here|
// unsigned __attribute__ ((visibility ("default"))) foo(unsigned);
// |] [here|
// #ccall foo , CUInt -> IO CUInt
// |]
//             matches [here|
// long long __attribute__ ((visibility ("default"))) foo(long long);
// |] [here|
// #ccall foo , CLLong -> IO CLLong
// |]

//     describe "primitive types" $ do
//         it "float" $
//             matches [here|
// float ordinary_float;
// |] [here|
// #globalvar ordinary_float , CFloat
// |]
//         it "double" $
//             matches [here|
// double ordinary_double;
// |] [here|
// #globalvar ordinary_double , CDouble
// |]
//         -- test disabled until https://ghc.haskell.org/trac/ghc/ticket/3353 is
//         -- resolved.
//         --
//         -- it "long double" $
//         --     matches [here|
//         -- long double ordinary_long_double;
//         -- |] [here|
//         -- #globalvar ordinary_long_double , CLongDouble
//         -- |]

//         it "char" $
//             matches [here|
// char ordinary_char;
// |] [here|
// #globalvar ordinary_char , CChar
// |]
//         it "signed char" $
//             matches [here|
// signed char signed_char;
// |] [here|
// #globalvar signed_char , CSChar
// |]
//         it "unsigned char" $
//             matches [here|
// unsigned char unsigned_char;
// |] [here|
// #globalvar unsigned_char , CUChar
// |]
//         it "short" $
//             matches [here|
// short ordinary_signed_short;
// |] [here|
// #globalvar ordinary_signed_short , CShort
// |]
//         it "signed short" $
//             matches [here|
// signed short explicit_signed_short;
// |] [here|
// #globalvar explicit_signed_short , CShort
// |]
//         it "unsigned short" $
//             matches [here|
// unsigned short unsigned_short;
// |] [here|
// #globalvar unsigned_short , CUShort
// |]
//         it "int" $
//             matches [here|
// int ordinary_signed_int;
// |] [here|
// #globalvar ordinary_signed_int , CInt
// |]
//         it "signed int" $
//             matches [here|
// signed int explicit_signed_int;
// |] [here|
// #globalvar explicit_signed_int , CInt
// |]
//         it "unsigned int" $
//             matches [here|
// unsigned int unsigned_int;
// |] [here|
// #globalvar unsigned_int , CUInt
// |]
//         it "long" $
//             matches [here|
// long ordinary_signed_long;
// |] [here|
// #globalvar ordinary_signed_long , CLong
// |]
//         it "signed long" $
//             matches [here|
// signed long explicit_signed_long;
// |] [here|
// #globalvar explicit_signed_long , CLong
// |]
//         it "unsigned long" $
//             matches [here|
// unsigned long unsigned_long;
// |] [here|
// #globalvar unsigned_long , CULong
// |]

//         it "long long" $ do
//             matches [here|
// long long ordinary_signed_long_long;
// |] [here|
// #globalvar ordinary_signed_long_long , CLLong
// |]
//         it "signed long long" $ do
//             matches [here|
// signed long long explicit_signed_long_long;
// |] [here|
// #globalvar explicit_signed_long_long , CLLong
// |]
//         it "unsigned long long" $
//             matches [here|
// unsigned long long unsigned_long_long;
// |] [here|
// #globalvar unsigned_long_long , CULLong
// |]

//     describe "pointers" $ do
//         describe "primitive types which cannot be signed" $ do
//             it "ordinary_void_pointer" $
//                 -- jww (2014-04-05): This is wrong!
//                 matches [here|
//         void* ordinary_void_pointer;
// |] [here|
// #globalvar ordinary_void_pointer , Ptr ()
// |]
//             it "ordinary_float_pointer" $
//                 matches [here|
//         float* ordinary_float_pointer;
// |] [here|
// #globalvar ordinary_float_pointer , Ptr CFloat
// |]
//             it "ordinary_double_pointer" $
//                 matches [here|
//         double* ordinary_double_pointer;
// |] [here|
// #globalvar ordinary_double_pointer , Ptr CDouble
// |]
// --             it "ordinary_long_double_pointer" $
// --                 matches [here|
// --         long double* ordinary_long_double_pointer;
// -- |] [here|
// -- |]
//         describe "types which can be signed" $ do
//             describe "char" $ do
//                 it "ordinary_char_pointer" $
//                     matches [here|
//           char *ordinary_char_pointer;
// |] [here|
// #globalvar ordinary_char_pointer , Ptr CChar
// |]
//                 it "signed_char_pointer" $
//                     matches [here|
//           signed char *signed_char_pointer;
// |] [here|
// #globalvar signed_char_pointer , Ptr CSChar
// |]
//                 it "unsigned_char_pointer" $
//                     matches [here|
//           unsigned char *unsigned_char_pointer;
// |] [here|
// #globalvar unsigned_char_pointer , Ptr CUChar
// |]
//             describe "short" $ do
//                 it "ordinary_signed_short_pointer" $
//                     matches [here|
//           short *ordinary_signed_short_pointer;
// |] [here|
// #globalvar ordinary_signed_short_pointer , Ptr CShort
// |]
//                 it "explicit_signed_short_pointer" $
//                     matches [here|
//           signed short *explicit_signed_short_pointer;
// |] [here|
// #globalvar explicit_signed_short_pointer , Ptr CShort
// |]
//                 it "unsigned_short_pointer" $
//                     matches [here|
//           unsigned short *unsigned_short_pointer;
// |] [here|
// #globalvar unsigned_short_pointer , Ptr CUShort
// |]
//             describe "int" $ do
//                 it "ordinary_signed_int_pointer" $
//                     matches [here|
//           int* ordinary_signed_int_pointer;
// |] [here|
// #globalvar ordinary_signed_int_pointer , Ptr CInt
// |]
//                 it "explicit_signed_int_pointer" $
//                     matches [here|
//           signed int* explicit_signed_int_pointer;
// |] [here|
// #globalvar explicit_signed_int_pointer , Ptr CInt
// |]
//                 it "unsigned_int_pointer" $
//                     matches [here|
//           unsigned int* unsigned_int_pointer;
// |] [here|
// #globalvar unsigned_int_pointer , Ptr CUInt
// |]
//             describe "long" $ do
//                 it "ordinary_signed_long_pointer" $
//                     matches [here|
//           long *ordinary_signed_long_pointer;
// |] [here|
// #globalvar ordinary_signed_long_pointer , Ptr CLong
// |]
//                 it "explicit_signed_long_pointer" $
//                     matches [here|
//           signed long *explicit_signed_long_pointer;
// |] [here|
// #globalvar explicit_signed_long_pointer , Ptr CLong
// |]
//                 it "unsigned_long_pointer" $
//                     matches [here|
//           unsigned long *unsigned_long_pointer;
// |] [here|
// #globalvar unsigned_long_pointer , Ptr CULong
// |]
//             describe "long long" $ do
//                 it "ordinary_signed_long_long_pointer" $
//                     matches [here|
//           long long* ordinary_signed_long_long_pointer;
// |] [here|
// #globalvar ordinary_signed_long_long_pointer , Ptr CLLong
// |]
//                 it "explicit_signed_long_long_pointer" $
//                     matches [here|
//           signed long long* explicit_signed_long_long_pointer;
// |] [here|
// #globalvar explicit_signed_long_long_pointer , Ptr CLLong
// |]
//                 it "unsigned_long_long_pointer" $
//                     matches [here|
//           unsigned long long* unsigned_long_long_pointer;
// |] [here|
// #globalvar unsigned_long_long_pointer , Ptr CULLong
// |]

//     describe "arrays" $ do
//         describe "primitive types which cannot be signed" $ do
//             it "ordinary_float_array" $
//                 matches [here|
//         float ordinary_float_array[10];
// |] [here|
// #globalarray ordinary_float_array , CFloat
// |]
//             it "ordinary_double_array" $
//                 matches [here|
//         double ordinary_double_array[10];
// |] [here|
// #globalarray ordinary_double_array , CDouble
// |]
//             it "ordinary_long_double_array" $
//                 matches [here|
//         long double ordinary_long_double_array[10];
// |] [here|
// #globalarray ordinary_long_double_array , CDouble
// |]
//         describe "types which can be signed" $ do
//             describe "char" $ do
//                 it "ordinary_signed_char_array" $
//                     matches [here|
//           char ordinary_signed_char_array[10];
// |] [here|
// #globalarray ordinary_signed_char_array , CChar
// |]
//                 it "explicit_signed_char_array" $
//                     matches [here|
//           signed char explicit_signed_char_array[10];
// |] [here|
// #globalarray explicit_signed_char_array , CSChar
// |]
//                 it "unsigned_char_array" $
//                     matches [here|
//           unsigned char unsigned_char_array[10];
// |] [here|
// #globalarray unsigned_char_array , CUChar
// |]
//             describe "short" $ do
//                 it "ordinary_signed_short_array" $
//                     matches [here|
//           short ordinary_signed_short_array[10];
// |] [here|
// #globalarray ordinary_signed_short_array , CShort
// |]
//                 it "explicit_signed_short_array" $
//                     matches [here|
//           signed short explicit_signed_short_array[10];
// |] [here|
// #globalarray explicit_signed_short_array , CShort
// |]
//                 it "unsigned_short_array" $
//                     matches [here|
//           unsigned short unsigned_short_array[10];
// |] [here|
// #globalarray unsigned_short_array , CUShort
// |]
//             describe "int" $ do
//                 it "ordinary_signed_int_array" $
//                     matches [here|
//           int ordinary_signed_int_array[10];
// |] [here|
// #globalarray ordinary_signed_int_array , CInt
// |]
//                 it "explicit_signed_int_array" $
//                     matches [here|
//           signed int explicit_signed_int_array[10];
// |] [here|
// #globalarray explicit_signed_int_array , CInt
// |]
//                 it "unsigned_int_array" $
//                     matches [here|
//           unsigned int unsigned_int_array[10];
// |] [here|
// #globalarray unsigned_int_array , CUInt
// |]
//             describe "long" $ do
//                 it "ordinary_signed_long_array" $
//                     matches [here|
//           long ordinary_signed_long_array[10];
// |] [here|
// #globalarray ordinary_signed_long_array , CLong
// |]
//                 it "explicit_signed_long_array" $
//                     matches [here|
//           signed long explicit_signed_long_array[10];
// |] [here|
// #globalarray explicit_signed_long_array , CLong
// |]
//                 it "unsigned_long_array" $
//                     matches [here|
//           unsigned long unsigned_long_array[10];
// |] [here|
// #globalarray unsigned_long_array , CULong
// |]
//             describe "long long" $ do
//                 it "ordinary_signed_long_long_array" $
//                     matches [here|
//           long long ordinary_signed_long_long_array[10];
// |] [here|
// #globalarray ordinary_signed_long_long_array , CLLong
// |]
//                 it "explicit_signed_long_long_array" $
//                     matches [here|
//           signed long long explicit_signed_long_long_array[10];
// |] [here|
// #globalarray explicit_signed_long_long_array , CLLong
// |]
//                 it "unsigned_long_long_array" $
//                     matches [here|
//           unsigned long long unsigned_long_long_array[10];
// |] [here|
// #globalarray unsigned_long_long_array , CULLong
// |]
//         describe "pointers" $ do
//             describe "primitive types which cannot be signed" $ do
//                 it "ordinary_void_pointer_array" $
//                     matches [here|
//           void* ordinary_void_pointer_array[10];
// |] [here|
// #globalarray ordinary_void_pointer_array , Ptr ()
// |]
//                 it "ordinary_float_pointer_array" $
//                     matches [here|
//           float* ordinary_float_pointer_array[10];
// |] [here|
// #globalarray ordinary_float_pointer_array , Ptr CFloat
// |]
//                 it "ordinary_double_pointer_array" $
//                     matches [here|
//           double* ordinary_double_pointer_array[10];
// |] [here|
// #globalarray ordinary_double_pointer_array , Ptr CDouble
// |]
//                 it "ordinary_long_double_pointer_array" $
//                     matches [here|
//           long double* ordinary_long_double_pointer_array[10];
// |] [here|
// #globalarray ordinary_long_double_pointer_array , Ptr CDouble
// |]
//             describe "types which can be signed" $ do
//                 describe "char" $ do
//                     it "ordinary_signed_char_pointer_array" $
//                         matches [here|
//             char *ordinary_signed_char_pointer_array[10];
// |] [here|
// #globalarray ordinary_signed_char_pointer_array , Ptr CChar
// |]
//                     it "explicit_signed_char_pointer_array" $
//                         matches [here|
//             signed char *explicit_signed_char_pointer_array[10];
// |] [here|
// #globalarray explicit_signed_char_pointer_array , Ptr CSChar
// |]
//                     it "unsigned_char_pointer_array" $
//                         matches [here|
//             unsigned char *unsigned_char_pointer_array[10];
// |] [here|
// #globalarray unsigned_char_pointer_array , Ptr CUChar
// |]
//                 describe "short" $ do
//                     it "ordinary_signed_short_pointer_array" $
//                         matches [here|
//             short *ordinary_signed_short_pointer_array[10];
// |] [here|
// #globalarray ordinary_signed_short_pointer_array , Ptr CShort
// |]
//                     it "explicit_signed_short_pointer_array" $
//                         matches [here|
//             signed short *explicit_signed_short_pointer_array[10];
// |] [here|
// #globalarray explicit_signed_short_pointer_array , Ptr CShort
// |]
//                     it "unsigned_short_pointer_array" $
//                         matches [here|
//             unsigned short *unsigned_short_pointer_array[10];
// |] [here|
// #globalarray unsigned_short_pointer_array , Ptr CUShort
// |]
//                 describe "int" $ do
//                     it "ordinary_signed_int_pointer_array" $
//                         matches [here|
//             int* ordinary_signed_int_pointer_array[10];
// |] [here|
// #globalarray ordinary_signed_int_pointer_array , Ptr CInt
// |]
//                     it "explicit_signed_int_pointer_array" $
//                         matches [here|
//             signed int* explicit_signed_int_pointer_array[10];
// |] [here|
// #globalarray explicit_signed_int_pointer_array , Ptr CInt
// |]
//                     it "unsigned_int_pointer_array" $
//                         matches [here|
//             unsigned int* unsigned_int_pointer_array[10];
// |] [here|
// #globalarray unsigned_int_pointer_array , Ptr CUInt
// |]
//                 describe "long" $ do
//                     it "ordinary_signed_long_pointer_array" $
//                         matches [here|
//             long *ordinary_signed_long_pointer_array[10];
// |] [here|
// #globalarray ordinary_signed_long_pointer_array , Ptr CLong
// |]
//                     it "explicit_signed_long_pointer_array" $
//                         matches [here|
//             signed long *explicit_signed_long_pointer_array[10];
// |] [here|
// #globalarray explicit_signed_long_pointer_array , Ptr CLong
// |]
//                     it "unsigned_long_pointer_array" $
//                         matches [here|
//             unsigned long *unsigned_long_pointer_array[10];
// |] [here|
// #globalarray unsigned_long_pointer_array , Ptr CULong
// |]
//                 describe "long long" $ do
//                     it "ordinary_signed_long_long_pointer_array" $
//                         matches [here|
//             long long* ordinary_signed_long_long_pointer_array[10];
// |] [here|
// #globalarray ordinary_signed_long_long_pointer_array , Ptr CLLong
// |]
//                     it "explicit_signed_long_long_pointer_array" $
//                         matches [here|
//             signed long long* explicit_signed_long_long_pointer_array[10];
// |] [here|
// #globalarray explicit_signed_long_long_pointer_array , Ptr CLLong
// |]
//                     it "unsigned_long_long_pointer_array" $
//                         matches [here|
//             unsigned long long* unsigned_long_long_pointer_array[10];
// |] [here|
// #globalarray unsigned_long_long_pointer_array , Ptr CULLong
// |]

//     describe "structs" $ do
//         describe "primitive types which cannot be signed" $ do
//             it "ordinary_float_struct" $
//                 matches [here|
//         struct ordinary_float_struct {float ordinary_float_member;};
// |] [here|
// {- struct ordinary_float_struct {
//     float ordinary_float_member;
// }; -}
// #starttype struct ordinary_float_struct
// #field ordinary_float_member , CFloat
// #stoptype
// |]
//             it "ordinary_double_struct" $
//                 matches [here|
//         struct ordinary_double_struct {double ordinary_double_member;};
// |] [here|
// {- struct ordinary_double_struct {
//     double ordinary_double_member;
// }; -}
// #starttype struct ordinary_double_struct
// #field ordinary_double_member , CDouble
// #stoptype
// |]
//             it "ordinary_long_double_struct" $
//                 matches [here|
//         struct ordinary_long_double_struct {long double ordinary_long_double_member;};
// |] [here|
// {- struct ordinary_long_double_struct {
//     long double ordinary_long_double_member;
// }; -}
// #starttype struct ordinary_long_double_struct
// #field ordinary_long_double_member , CDouble
// #stoptype
// |]
//         describe "types which can be signed" $ do
//             describe "char" $ do
//                 it "ordinary_signed_char_struct" $
//                     matches [here|
//           struct ordinary_signed_char_struct {char ordinary_signed_char_member;};
// |] [here|
// {- struct ordinary_signed_char_struct {
//     char ordinary_signed_char_member;
// }; -}
// #starttype struct ordinary_signed_char_struct
// #field ordinary_signed_char_member , CChar
// #stoptype
// |]
//                 it "explicit_signed_char_struct" $
//                     matches [here|
//           struct explicit_signed_char_struct {signed char explicit_signed_char_member;};
// |] [here|
// {- struct explicit_signed_char_struct {
//     signed char explicit_signed_char_member;
// }; -}
// #starttype struct explicit_signed_char_struct
// #field explicit_signed_char_member , CSChar
// #stoptype
// |]
//                 it "unsigned_char_struct" $
//                     matches [here|
//           struct unsigned_char_struct {unsigned char unsigned_char_member;};
// |] [here|
// {- struct unsigned_char_struct {
//     unsigned char unsigned_char_member;
// }; -}
// #starttype struct unsigned_char_struct
// #field unsigned_char_member , CUChar
// #stoptype
// |]
//             describe "short" $ do
//                 it "ordinary_signed_short_struct" $
//                     matches [here|
//           struct ordinary_signed_short_struct {short ordinary_signed_short_member;};
// |] [here|
// {- struct ordinary_signed_short_struct {
//     short ordinary_signed_short_member;
// }; -}
// #starttype struct ordinary_signed_short_struct
// #field ordinary_signed_short_member , CShort
// #stoptype
// |]
//                 it "explicit_signed_short_struct" $
//                     matches [here|
//           struct explicit_signed_short_struct {signed short explicit_signed_short_member;};
// |] [here|
// {- struct explicit_signed_short_struct {
//     signed short explicit_signed_short_member;
// }; -}
// #starttype struct explicit_signed_short_struct
// #field explicit_signed_short_member , CShort
// #stoptype
// |]
//                 it "unsigned_short_struct" $
//                     matches [here|
//           struct unsigned_short_struct {unsigned short unsigned_short_member;};
// |] [here|
// {- struct unsigned_short_struct {
//     unsigned short unsigned_short_member;
// }; -}
// #starttype struct unsigned_short_struct
// #field unsigned_short_member , CUShort
// #stoptype
// |]
//             describe "int" $ do
//                 it "ordinary_signed_int_struct" $
//                     matches [here|
//           struct ordinary_signed_int_struct {int ordinary_signed_int_member;};
// |] [here|
// {- struct ordinary_signed_int_struct {
//     int ordinary_signed_int_member;
// }; -}
// #starttype struct ordinary_signed_int_struct
// #field ordinary_signed_int_member , CInt
// #stoptype
// |]
//                 it "explicit_signed_int_struct" $
//                     matches [here|
//           struct explicit_signed_int_struct {signed int explicit_signed_int_member;};
// |] [here|
// {- struct explicit_signed_int_struct {
//     signed int explicit_signed_int_member;
// }; -}
// #starttype struct explicit_signed_int_struct
// #field explicit_signed_int_member , CInt
// #stoptype
// |]
//                 it "unsigned_int_struct" $
//                     matches [here|
//           struct unsigned_int_struct {unsigned int unsigned_int_member;};
// |] [here|
// {- struct unsigned_int_struct {
//     unsigned int unsigned_int_member;
// }; -}
// #starttype struct unsigned_int_struct
// #field unsigned_int_member , CUInt
// #stoptype
// |]
//             describe "long" $ do
//                 it "ordinary_signed_long_struct" $
//                     matches [here|
//           struct ordinary_signed_long_struct {long ordinary_signed_long_member;};
// |] [here|
// {- struct ordinary_signed_long_struct {
//     long ordinary_signed_long_member;
// }; -}
// #starttype struct ordinary_signed_long_struct
// #field ordinary_signed_long_member , CLong
// #stoptype
// |]
//                 it "explicit_signed_long_struct" $
//                     matches [here|
//           struct explicit_signed_long_struct {signed long explicit_signed_long_member;};
// |] [here|
// {- struct explicit_signed_long_struct {
//     signed long explicit_signed_long_member;
// }; -}
// #starttype struct explicit_signed_long_struct
// #field explicit_signed_long_member , CLong
// #stoptype
// |]
//                 it "unsigned_long_struct" $
//                     matches [here|
//           struct unsigned_long_struct {unsigned long unsigned_long_member;};
// |] [here|
// {- struct unsigned_long_struct {
//     unsigned long unsigned_long_member;
// }; -}
// #starttype struct unsigned_long_struct
// #field unsigned_long_member , CULong
// #stoptype
// |]
//             describe "long long" $ do
//                 it "ordinary_signed_long_long_struct" $
//                     matches [here|
//           struct ordinary_signed_long_long_struct {long long ordinary_signed_long_long_member;};
// |] [here|
// {- struct ordinary_signed_long_long_struct {
//     long long ordinary_signed_long_long_member;
// }; -}
// #starttype struct ordinary_signed_long_long_struct
// #field ordinary_signed_long_long_member , CLLong
// #stoptype
// |]
//                 it "explicit_signed_long_long_struct" $
//                     matches [here|
//           struct explicit_signed_long_long_struct {signed long long explicit_signed_long_long_member;};
// |] [here|
// {- struct explicit_signed_long_long_struct {
//     signed long long explicit_signed_long_long_member;
// }; -}
// #starttype struct explicit_signed_long_long_struct
// #field explicit_signed_long_long_member , CLLong
// #stoptype
// |]
//                 it "unsigned_long_long_struct" $
//                     matches [here|
//           struct unsigned_long_long_struct {unsigned long long unsigned_long_long_member;};
// |] [here|
// {- struct unsigned_long_long_struct {
//     unsigned long long unsigned_long_long_member;
// }; -}
// #starttype struct unsigned_long_long_struct
// #field unsigned_long_long_member , CULLong
// #stoptype
// |]
//         describe "pointers" $ do
//             describe "primitive types which cannot be signed" $ do
//                 it "ordinary_void_pointer_struct" $
//                     matches [here|
//           struct ordinary_void_pointer_struct {void* ordinary_void_pointer_member;};
// |] [here|
// {- struct ordinary_void_pointer_struct {
//     void * ordinary_void_pointer_member;
// }; -}
// #starttype struct ordinary_void_pointer_struct
// #field ordinary_void_pointer_member , Ptr ()
// #stoptype
// |]
//                 it "ordinary_float_pointer_struct" $
//                     matches [here|
//           struct ordinary_float_pointer_struct {float* ordinary_float_pointer_member;};
// |] [here|
// {- struct ordinary_float_pointer_struct {
//     float * ordinary_float_pointer_member;
// }; -}
// #starttype struct ordinary_float_pointer_struct
// #field ordinary_float_pointer_member , Ptr CFloat
// #stoptype
// |]
//                 it "ordinary_double_pointer_struct" $
//                     matches [here|
//           struct ordinary_double_pointer_struct {double* ordinary_double_pointer_member;};
// |] [here|
// {- struct ordinary_double_pointer_struct {
//     double * ordinary_double_pointer_member;
// }; -}
// #starttype struct ordinary_double_pointer_struct
// #field ordinary_double_pointer_member , Ptr CDouble
// #stoptype
// |]
//                 it "ordinary_long_double_pointer_struct" $
//                     matches [here|
//           struct ordinary_long_double_pointer_struct {long double* ordinary_long_double_pointer_member;};
// |] [here|
// {- struct ordinary_long_double_pointer_struct {
//     long double * ordinary_long_double_pointer_member;
// }; -}
// #starttype struct ordinary_long_double_pointer_struct
// #field ordinary_long_double_pointer_member , Ptr CDouble
// #stoptype
// |]
//             describe "types which can be signed" $ do
//                 describe "char" $ do
// --                     it "ordinary_signed_char_pointer_struct" $
// --                         matches [here|
// --             struct ordinary_signed_char_pointer_struct {char *ordinary_signed_char_pointer_member;};
// -- |] [here|
// -- {- struct ordinary_signed_char_pointer_struct {
// --     char * ordinary_signed_char_pointer_member;
// -- }; -}
// -- #starttype struct ordinary_signed_char_pointer_struct
// -- #field ordinary_signed_char_pointer_member , Ptr CString
// -- #stoptype
// -- |]
//                     it "explicit_signed_char_pointer_struct" $
//                         matches [here|
//             struct explicit_signed_char_pointer_struct {signed char *explicit_signed_char_pointer_member;};
// |] [here|
// {- struct explicit_signed_char_pointer_struct {
//     signed char * explicit_signed_char_pointer_member;
// }; -}
// #starttype struct explicit_signed_char_pointer_struct
// #field explicit_signed_char_pointer_member , Ptr CSChar
// #stoptype
// |]
//                     it "unsigned_char_pointer_struct" $
//                         matches [here|
//             struct unsigned_char_pointer_struct {unsigned char *unsigned_char_pointer_member;};
// |] [here|
// {- struct unsigned_char_pointer_struct {
//     unsigned char * unsigned_char_pointer_member;
// }; -}
// #starttype struct unsigned_char_pointer_struct
// #field unsigned_char_pointer_member , Ptr CUChar
// #stoptype
// |]
//                 describe "short" $ do
//                     it "ordinary_signed_short_pointer_struct" $
//                         matches [here|
//             struct ordinary_signed_short_pointer_struct {short *ordinary_signed_short_pointer_member;};
// |] [here|
// {- struct ordinary_signed_short_pointer_struct {
//     short * ordinary_signed_short_pointer_member;
// }; -}
// #starttype struct ordinary_signed_short_pointer_struct
// #field ordinary_signed_short_pointer_member , Ptr CShort
// #stoptype
// |]
//                     it "explicit_signed_short_pointer_struct" $
//                         matches [here|
//             struct explicit_signed_short_pointer_struct {signed short *explicit_signed_short_pointer_member;};
// |] [here|
// {- struct explicit_signed_short_pointer_struct {
//     signed short * explicit_signed_short_pointer_member;
// }; -}
// #starttype struct explicit_signed_short_pointer_struct
// #field explicit_signed_short_pointer_member , Ptr CShort
// #stoptype
// |]
//                     it "unsigned_short_pointer_struct" $
//                         matches [here|
//             struct unsigned_short_pointer_struct {unsigned short *unsigned_short_pointer_member;};
// |] [here|
// {- struct unsigned_short_pointer_struct {
//     unsigned short * unsigned_short_pointer_member;
// }; -}
// #starttype struct unsigned_short_pointer_struct
// #field unsigned_short_pointer_member , Ptr CUShort
// #stoptype
// |]
//                 describe "int" $ do
//                     it "ordinary_signed_int_pointer_struct" $
//                         matches [here|
//             struct ordinary_signed_int_pointer_struct {int* ordinary_signed_int_pointer_member;};
// |] [here|
// {- struct ordinary_signed_int_pointer_struct {
//     int * ordinary_signed_int_pointer_member;
// }; -}
// #starttype struct ordinary_signed_int_pointer_struct
// #field ordinary_signed_int_pointer_member , Ptr CInt
// #stoptype
// |]
//                     it "explicit_signed_int_pointer_struct" $
//                         matches [here|
//             struct explicit_signed_int_pointer_struct {signed int* explicit_signed_int_pointer_member;};
// |] [here|
// {- struct explicit_signed_int_pointer_struct {
//     signed int * explicit_signed_int_pointer_member;
// }; -}
// #starttype struct explicit_signed_int_pointer_struct
// #field explicit_signed_int_pointer_member , Ptr CInt
// #stoptype
// |]
//                     it "unsigned_int_pointer_struct" $
//                         matches [here|
//             struct unsigned_int_pointer_struct {unsigned int* unsigned_int_pointer_member;};
// |] [here|
// {- struct unsigned_int_pointer_struct {
//     unsigned int * unsigned_int_pointer_member;
// }; -}
// #starttype struct unsigned_int_pointer_struct
// #field unsigned_int_pointer_member , Ptr CUInt
// #stoptype
// |]
//                 describe "long" $ do
//                     it "ordinary_signed_long_pointer_struct" $
//                         matches [here|
//             struct ordinary_signed_long_pointer_struct {long *ordinary_signed_long_pointer_member;};
// |] [here|
// {- struct ordinary_signed_long_pointer_struct {
//     long * ordinary_signed_long_pointer_member;
// }; -}
// #starttype struct ordinary_signed_long_pointer_struct
// #field ordinary_signed_long_pointer_member , Ptr CLong
// #stoptype
// |]
//                     it "explicit_signed_long_pointer_struct" $
//                         matches [here|
//             struct explicit_signed_long_pointer_struct {signed long *explicit_signed_long_pointer_member;};
// |] [here|
// {- struct explicit_signed_long_pointer_struct {
//     signed long * explicit_signed_long_pointer_member;
// }; -}
// #starttype struct explicit_signed_long_pointer_struct
// #field explicit_signed_long_pointer_member , Ptr CLong
// #stoptype
// |]
//                     it "unsigned_long_pointer_struct" $
//                         matches [here|
//             struct unsigned_long_pointer_struct {unsigned long *unsigned_long_pointer_member;};
// |] [here|
// {- struct unsigned_long_pointer_struct {
//     unsigned long * unsigned_long_pointer_member;
// }; -}
// #starttype struct unsigned_long_pointer_struct
// #field unsigned_long_pointer_member , Ptr CULong
// #stoptype
// |]
//                 describe "long long" $ do
//                     it "ordinary_signed_long_long_pointer_struct" $
//                         matches [here|
//             struct ordinary_signed_long_long_pointer_struct {long long* ordinary_signed_long_long_pointer_member;};
// |] [here|
// {- struct ordinary_signed_long_long_pointer_struct {
//     long long * ordinary_signed_long_long_pointer_member;
// }; -}
// #starttype struct ordinary_signed_long_long_pointer_struct
// #field ordinary_signed_long_long_pointer_member , Ptr CLLong
// #stoptype
// |]
//                     it "explicit_signed_long_long_pointer_struct" $
//                         matches [here|
//             struct explicit_signed_long_long_pointer_struct {signed long long* explicit_signed_long_long_pointer_member;};
// |] [here|
// {- struct explicit_signed_long_long_pointer_struct {
//     signed long long * explicit_signed_long_long_pointer_member;
// }; -}
// #starttype struct explicit_signed_long_long_pointer_struct
// #field explicit_signed_long_long_pointer_member , Ptr CLLong
// #stoptype
// |]
//                     it "unsigned_long_long_pointer_struct" $
//                         matches [here|
//             struct unsigned_long_long_pointer_struct {unsigned long long* unsigned_long_long_pointer_member;};
// |] [here|
// {- struct unsigned_long_long_pointer_struct {
//     unsigned long long * unsigned_long_long_pointer_member;
// }; -}
// #starttype struct unsigned_long_long_pointer_struct
// #field unsigned_long_long_pointer_member , Ptr CULLong
// #stoptype
// |]
//             describe "arrays" $ do
//                 describe "primitive types which cannot be signed" $ do
//                     it "ordinary_float_array_struct" $
//                         matches [here|
//           struct ordinary_float_array_struct {float ordinary_float_array_member[10];};
// |] [here|
// {- struct ordinary_float_array_struct {
//     float ordinary_float_array_member[10];
// }; -}
// #starttype struct ordinary_float_array_struct
// #array_field ordinary_float_array_member , CFloat
// #stoptype
// |]
//                     it "ordinary_double_array_struct" $
//                         matches [here|
//           struct ordinary_double_array_struct {double ordinary_double_array_member[10];};
// |] [here|
// {- struct ordinary_double_array_struct {
//     double ordinary_double_array_member[10];
// }; -}
// #starttype struct ordinary_double_array_struct
// #array_field ordinary_double_array_member , CDouble
// #stoptype
// |]
//                     it "ordinary_long_double_array_struct" $
//                         matches [here|
//           struct ordinary_long_double_array_struct {long double ordinary_long_double_array_member[10];};
// |] [here|
// {- struct ordinary_long_double_array_struct {
//     long double ordinary_long_double_array_member[10];
// }; -}
// #starttype struct ordinary_long_double_array_struct
// #array_field ordinary_long_double_array_member , CDouble
// #stoptype
// |]
//             describe "types which can be signed" $ do
//                 describe "char" $ do
//                     it "ordinary_signed_char_array_struct" $
//                         matches [here|
//             struct ordinary_signed_char_array_struct {char ordinary_signed_char_array_member[10];};
// |] [here|
// {- struct ordinary_signed_char_array_struct {
//     char ordinary_signed_char_array_member[10];
// }; -}
// #starttype struct ordinary_signed_char_array_struct
// #array_field ordinary_signed_char_array_member , CChar
// #stoptype
// |]
//                     it "explicit_signed_char_array_struct" $
//                         matches [here|
//             struct explicit_signed_char_array_struct {signed char explicit_signed_char_array_member[10];};
// |] [here|
// {- struct explicit_signed_char_array_struct {
//     signed char explicit_signed_char_array_member[10];
// }; -}
// #starttype struct explicit_signed_char_array_struct
// #array_field explicit_signed_char_array_member , CSChar
// #stoptype
// |]
//                     it "unsigned_char_array_struct" $
//                         matches [here|
//             struct unsigned_char_array_struct {unsigned char unsigned_char_array_member[10];};
// |] [here|
// {- struct unsigned_char_array_struct {
//     unsigned char unsigned_char_array_member[10];
// }; -}
// #starttype struct unsigned_char_array_struct
// #array_field unsigned_char_array_member , CUChar
// #stoptype
// |]
//                 describe "short" $ do
//                     it "ordinary_signed_short_array_struct" $
//                         matches [here|
//             struct ordinary_signed_short_array_struct {short ordinary_signed_short_array_member[10];};
// |] [here|
// {- struct ordinary_signed_short_array_struct {
//     short ordinary_signed_short_array_member[10];
// }; -}
// #starttype struct ordinary_signed_short_array_struct
// #array_field ordinary_signed_short_array_member , CShort
// #stoptype
// |]
//                     it "explicit_signed_short_array_struct" $
//                         matches [here|
//             struct explicit_signed_short_array_struct {signed short explicit_signed_short_array_member[10];};
// |] [here|
// {- struct explicit_signed_short_array_struct {
//     signed short explicit_signed_short_array_member[10];
// }; -}
// #starttype struct explicit_signed_short_array_struct
// #array_field explicit_signed_short_array_member , CShort
// #stoptype
// |]
//                     it "unsigned_short_array_struct" $
//                         matches [here|
//             struct unsigned_short_array_struct {unsigned short unsigned_short_array_member[10];};
// |] [here|
// {- struct unsigned_short_array_struct {
//     unsigned short unsigned_short_array_member[10];
// }; -}
// #starttype struct unsigned_short_array_struct
// #array_field unsigned_short_array_member , CUShort
// #stoptype
// |]
//                 describe "int" $ do
//                     it "ordinary_signed_int_array_struct" $
//                         matches [here|
//             struct ordinary_signed_int_array_struct {int ordinary_signed_int_array_member[10];};
// |] [here|
// {- struct ordinary_signed_int_array_struct {
//     int ordinary_signed_int_array_member[10];
// }; -}
// #starttype struct ordinary_signed_int_array_struct
// #array_field ordinary_signed_int_array_member , CInt
// #stoptype
// |]
//                     it "explicit_signed_int_array_struct" $
//                         matches [here|
//             struct explicit_signed_int_array_struct {signed int explicit_signed_int_array_member[10];};
// |] [here|
// {- struct explicit_signed_int_array_struct {
//     signed int explicit_signed_int_array_member[10];
// }; -}
// #starttype struct explicit_signed_int_array_struct
// #array_field explicit_signed_int_array_member , CInt
// #stoptype
// |]
//                     it "unsigned_int_array_struct" $
//                         matches [here|
//             struct unsigned_int_array_struct {unsigned int unsigned_int_array_member[10];};
// |] [here|
// {- struct unsigned_int_array_struct {
//     unsigned int unsigned_int_array_member[10];
// }; -}
// #starttype struct unsigned_int_array_struct
// #array_field unsigned_int_array_member , CUInt
// #stoptype
// |]
//                 describe "long" $ do
//                     it "ordinary_signed_long_array_struct" $
//                         matches [here|
//             struct ordinary_signed_long_array_struct {long ordinary_signed_long_array_member[10];};
// |] [here|
// {- struct ordinary_signed_long_array_struct {
//     long ordinary_signed_long_array_member[10];
// }; -}
// #starttype struct ordinary_signed_long_array_struct
// #array_field ordinary_signed_long_array_member , CLong
// #stoptype
// |]
//                     it "explicit_signed_long_array_struct" $
//                         matches [here|
//             struct explicit_signed_long_array_struct {signed long explicit_signed_long_array_member[10];};
// |] [here|
// {- struct explicit_signed_long_array_struct {
//     signed long explicit_signed_long_array_member[10];
// }; -}
// #starttype struct explicit_signed_long_array_struct
// #array_field explicit_signed_long_array_member , CLong
// #stoptype
// |]
//                     it "unsigned_long_array_struct" $
//                         matches [here|
//             struct unsigned_long_array_struct {unsigned long unsigned_long_array_member[10];};
// |] [here|
// {- struct unsigned_long_array_struct {
//     unsigned long unsigned_long_array_member[10];
// }; -}
// #starttype struct unsigned_long_array_struct
// #array_field unsigned_long_array_member , CULong
// #stoptype
// |]
//                 describe "long long" $ do
//                     it "ordinary_signed_long_long_array_struct" $
//                         matches [here|
//             struct ordinary_signed_long_long_array_struct {long long ordinary_signed_long_long_array_member[10];};
// |] [here|
// {- struct ordinary_signed_long_long_array_struct {
//     long long ordinary_signed_long_long_array_member[10];
// }; -}
// #starttype struct ordinary_signed_long_long_array_struct
// #array_field ordinary_signed_long_long_array_member , CLLong
// #stoptype
// |]
//                     it "explicit_signed_long_long_array_struct" $
//                         matches [here|
//             struct explicit_signed_long_long_array_struct {signed long long explicit_signed_long_long_array_member[10];};
// |] [here|
// {- struct explicit_signed_long_long_array_struct {
//     signed long long explicit_signed_long_long_array_member[10];
// }; -}
// #starttype struct explicit_signed_long_long_array_struct
// #array_field explicit_signed_long_long_array_member , CLLong
// #stoptype
// |]
//                     it "unsigned_long_long_array_struct" $
//                         matches [here|
//             struct unsigned_long_long_array_struct {unsigned long long unsigned_long_long_array_member[10];};
// |] [here|
// {- struct unsigned_long_long_array_struct {
//     unsigned long long unsigned_long_long_array_member[10];
// }; -}
// #starttype struct unsigned_long_long_array_struct
// #array_field unsigned_long_long_array_member , CULLong
// #stoptype
// |]
//             describe "pointers" $ do
//                 describe "primitive types which cannot be signed" $ do
//                     it "ordinary_void_pointer_array_struct" $
//                         matches [here|
//             struct ordinary_void_pointer_array_struct {void* ordinary_void_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_void_pointer_array_struct {
//     void * ordinary_void_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_void_pointer_array_struct
// #array_field ordinary_void_pointer_array_member , Ptr ()
// #stoptype
// |]
//                     it "ordinary_float_pointer_array_struct" $
//                         matches [here|
//             struct ordinary_float_pointer_array_struct {float* ordinary_float_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_float_pointer_array_struct {
//     float * ordinary_float_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_float_pointer_array_struct
// #array_field ordinary_float_pointer_array_member , Ptr CFloat
// #stoptype
// |]
//                     it "ordinary_double_pointer_array_struct" $
//                         matches [here|
//             struct ordinary_double_pointer_array_struct {double* ordinary_double_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_double_pointer_array_struct {
//     double * ordinary_double_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_double_pointer_array_struct
// #array_field ordinary_double_pointer_array_member , Ptr CDouble
// #stoptype
// |]
//                     it "ordinary_long_double_pointer_array_struct" $
//                         matches [here|
//             struct ordinary_long_double_pointer_array_struct {long double* ordinary_long_double_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_long_double_pointer_array_struct {
//     long double * ordinary_long_double_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_long_double_pointer_array_struct
// #array_field ordinary_long_double_pointer_array_member , Ptr CDouble
// #stoptype
// |]
//                 describe "types which can be signed" $ do
//                     describe "char" $ do
// --                         it "ordinary_signed_char_pointer_array_struct" $
// --                             matches [here|
// --               struct ordinary_signed_char_pointer_array_struct {char *ordinary_signed_char_pointer_array_member[10];};
// -- |] [here|
// -- {- struct ordinary_signed_char_pointer_array_struct {
// --     char * ordinary_signed_char_pointer_array_member[10];
// -- }; -}
// -- #starttype struct ordinary_signed_char_pointer_array_struct
// -- #array_field ordinary_signed_char_pointer_array_member , Ptr CString
// -- #stoptype
// -- |]
//                         it "explicit_signed_char_pointer_array_struct" $
//                             matches [here|
//               struct explicit_signed_char_pointer_array_struct {signed char *explicit_signed_char_pointer_array_member[10];};
// |] [here|
// {- struct explicit_signed_char_pointer_array_struct {
//     signed char * explicit_signed_char_pointer_array_member[10];
// }; -}
// #starttype struct explicit_signed_char_pointer_array_struct
// #array_field explicit_signed_char_pointer_array_member , Ptr CSChar
// #stoptype
// |]
//                         it "unsigned_char_pointer_array_struct" $
//                             matches [here|
//               struct unsigned_char_pointer_array_struct {unsigned char *unsigned_char_pointer_array_member[10];};
// |] [here|
// {- struct unsigned_char_pointer_array_struct {
//     unsigned char * unsigned_char_pointer_array_member[10];
// }; -}
// #starttype struct unsigned_char_pointer_array_struct
// #array_field unsigned_char_pointer_array_member , Ptr CUChar
// #stoptype
// |]
//                     describe "short" $ do
//                         it "ordinary_signed_short_pointer_array_struct" $
//                             matches [here|
//               struct ordinary_signed_short_pointer_array_struct {short *ordinary_signed_short_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_signed_short_pointer_array_struct {
//     short * ordinary_signed_short_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_signed_short_pointer_array_struct
// #array_field ordinary_signed_short_pointer_array_member , Ptr CShort
// #stoptype
// |]
//                         it "explicit_signed_short_pointer_array_struct" $
//                             matches [here|
//               struct explicit_signed_short_pointer_array_struct {signed short *explicit_signed_short_pointer_array_member[10];};
// |] [here|
// {- struct explicit_signed_short_pointer_array_struct {
//     signed short * explicit_signed_short_pointer_array_member[10];
// }; -}
// #starttype struct explicit_signed_short_pointer_array_struct
// #array_field explicit_signed_short_pointer_array_member , Ptr CShort
// #stoptype
// |]
//                         it "unsigned_short_pointer_array_struct" $
//                             matches [here|
//               struct unsigned_short_pointer_array_struct {unsigned short *unsigned_short_pointer_array_member[10];};
// |] [here|
// {- struct unsigned_short_pointer_array_struct {
//     unsigned short * unsigned_short_pointer_array_member[10];
// }; -}
// #starttype struct unsigned_short_pointer_array_struct
// #array_field unsigned_short_pointer_array_member , Ptr CUShort
// #stoptype
// |]
//                     describe "int" $ do
//                         it "ordinary_signed_int_pointer_array_struct" $
//                             matches [here|
//               struct ordinary_signed_int_pointer_array_struct {int* ordinary_signed_int_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_signed_int_pointer_array_struct {
//     int * ordinary_signed_int_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_signed_int_pointer_array_struct
// #array_field ordinary_signed_int_pointer_array_member , Ptr CInt
// #stoptype
// |]
//                         it "explicit_signed_int_pointer_array_struct" $
//                             matches [here|
//               struct explicit_signed_int_pointer_array_struct {signed int* explicit_signed_int_pointer_array_member[10];};
// |] [here|
// {- struct explicit_signed_int_pointer_array_struct {
//     signed int * explicit_signed_int_pointer_array_member[10];
// }; -}
// #starttype struct explicit_signed_int_pointer_array_struct
// #array_field explicit_signed_int_pointer_array_member , Ptr CInt
// #stoptype
// |]
//                         it "unsigned_int_pointer_array_struct" $
//                             matches [here|
//               struct unsigned_int_pointer_array_struct {unsigned int* unsigned_int_pointer_array_member[10];};
// |] [here|
// {- struct unsigned_int_pointer_array_struct {
//     unsigned int * unsigned_int_pointer_array_member[10];
// }; -}
// #starttype struct unsigned_int_pointer_array_struct
// #array_field unsigned_int_pointer_array_member , Ptr CUInt
// #stoptype
// |]
//                     describe "long" $ do
//                         it "ordinary_signed_long_pointer_array_struct" $
//                             matches [here|
//               struct ordinary_signed_long_pointer_array_struct {long *ordinary_signed_long_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_signed_long_pointer_array_struct {
//     long * ordinary_signed_long_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_signed_long_pointer_array_struct
// #array_field ordinary_signed_long_pointer_array_member , Ptr CLong
// #stoptype
// |]
//                         it "explicit_signed_long_pointer_array_struct" $
//                             matches [here|
//               struct explicit_signed_long_pointer_array_struct {signed long *explicit_signed_long_pointer_array_member[10];};
// |] [here|
// {- struct explicit_signed_long_pointer_array_struct {
//     signed long * explicit_signed_long_pointer_array_member[10];
// }; -}
// #starttype struct explicit_signed_long_pointer_array_struct
// #array_field explicit_signed_long_pointer_array_member , Ptr CLong
// #stoptype
// |]
//                         it "unsigned_long_pointer_array_struct" $
//                             matches [here|
//               struct unsigned_long_pointer_array_struct {unsigned long *unsigned_long_pointer_array_member[10];};
// |] [here|
// {- struct unsigned_long_pointer_array_struct {
//     unsigned long * unsigned_long_pointer_array_member[10];
// }; -}
// #starttype struct unsigned_long_pointer_array_struct
// #array_field unsigned_long_pointer_array_member , Ptr CULong
// #stoptype
// |]
//                     describe "long long" $ do
//                         it "ordinary_signed_long_long_pointer_array_struct" $
//                             matches [here|
//               struct ordinary_signed_long_long_pointer_array_struct {long long* ordinary_signed_long_long_pointer_array_member[10];};
// |] [here|
// {- struct ordinary_signed_long_long_pointer_array_struct {
//     long long * ordinary_signed_long_long_pointer_array_member[10];
// }; -}
// #starttype struct ordinary_signed_long_long_pointer_array_struct
// #array_field ordinary_signed_long_long_pointer_array_member , Ptr CLLong
// #stoptype
// |]
//                         it "explicit_signed_long_long_pointer_array_struct" $
//                             matches [here|
//               struct explicit_signed_long_long_pointer_array_struct {signed long long* explicit_signed_long_long_pointer_array_member[10];};
// |] [here|
// {- struct explicit_signed_long_long_pointer_array_struct {
//     signed long long * explicit_signed_long_long_pointer_array_member[10];
// }; -}
// #starttype struct explicit_signed_long_long_pointer_array_struct
// #array_field explicit_signed_long_long_pointer_array_member , Ptr CLLong
// #stoptype
// |]
//                         it "unsigned_long_long_pointer_array_struct" $
//                             matches [here|
//               struct unsigned_long_long_pointer_array_struct {unsigned long long* unsigned_long_long_pointer_array_member[10];};
// |] [here|
// {- struct unsigned_long_long_pointer_array_struct {
//     unsigned long long * unsigned_long_long_pointer_array_member[10];
// }; -}
// #starttype struct unsigned_long_long_pointer_array_struct
// #array_field unsigned_long_long_pointer_array_member , Ptr CULLong
// #stoptype
// |]

//     describe "sanity check" $ do
//         it "maps a typedef" $
//             matches [here|
// typedef int an_int;
// |] [here|
// {- typedef int an_int; -}
// #synonym_t an_int , CInt
// |]

//         it "processes smoke.h" $
//             matches [here|
// typedef unsigned int uint;
// typedef unsigned long size_t;

// void    foo1(void);
// void    foo2(int);
// void    foo3(int, int);
// int     foo4(void);
// char    foo5(int);
// char *  foo6(int, int);
// char *  foo7(char *);
// char *  foo8(char * b);
// char *  foo9(char * (*b)(void));
// char *  foo10(char * (*b)(int));
// void *  foo11(void * (*b)(void));
// void *  foo12(void * (*b)(int));
// char *  foo13(char []);
// char *  foo14(char b[]);
// char *  foo15(char b[5]);
// char *  foo16(int);
// int     foo17(char ***);
// int     foo18(unsigned);
// int     foo19(unsigned int);
// int     foo20(uint);
// int     foo21(int (*)(int));
// int     foo22(int *(*)(int));
// int     foo23(int **(*)(int));
// int     foo24(int ***(*)(int));
// int *   foo25(int);
// int **  foo26(int);
// int *** foo27(int);
// int *** foo28(size_t);

// struct bar1_t {
//   void *  a;
//   int     b;
//   char    c;
//   char *  d;
//   char *  (*e)(void);
//   void    (*f)(void *);
//   int *   (*g)(void *);
//   int **  (*h)(void *);
//   int *** (*i)(void *);
//   char    j[2];

//   struct bar1_t * k;
// };

// typedef struct bar2_t {
//   int a;
// } bar2_t;

// typedef struct {
//   int a;
// } bar3_t;

// enum {
//   BAZ1 = 1
// };

// typedef enum {
//   BAZ2 = 1
// } baz2_t;

// enum baz3_t {
//   BAZ3 = 1
// };

// typedef enum baz4_t {
//   BAZ4 = 1
// } baz4_t;

// extern int global;

// inline int inline_foo(int a, int * b, const int c, const int * d,
//                       const int ** e, const int * const * f, size_t g) {
//   return 10;
// }
// |] [here|
// {- typedef unsigned int uint; -}
// #synonym_t uint , CUInt
// {- typedef unsigned long size_t; -}
// #synonym_t size_t , CULong
// #ccall foo1 , IO ()
// #ccall foo2 , CInt -> IO ()
// #ccall foo3 , CInt -> CInt -> IO ()
// #ccall foo4 , IO CInt
// #ccall foo5 , CInt -> IO CChar
// #ccall foo6 , CInt -> CInt -> IO CString
// #ccall foo7 , CString -> IO CString
// #ccall foo8 , CString -> IO CString
// #ccall foo9 , FunPtr CString -> IO CString
// #ccall foo10 , FunPtr (CInt -> CString) -> IO CString
// #ccall foo11 , FunPtr (Ptr ()) -> IO (Ptr ())
// #ccall foo12 , FunPtr (CInt -> Ptr ()) -> IO (Ptr ())
// #ccall foo13 , Ptr CChar -> IO CString
// #ccall foo14 , Ptr CChar -> IO CString
// #ccall foo15 , Ptr CChar -> IO CString
// #ccall foo16 , CInt -> IO CString
// #ccall foo17 , Ptr (Ptr CString) -> IO CInt
// #ccall foo18 , CUInt -> IO CInt
// #ccall foo19 , CUInt -> IO CInt
// #ccall foo20 , CUInt -> IO CInt
// #ccall foo21 , FunPtr (CInt -> CInt) -> IO CInt
// #ccall foo22 , FunPtr (CInt -> Ptr CInt) -> IO CInt
// #ccall foo23 , FunPtr (CInt -> Ptr (Ptr CInt)) -> IO CInt
// #ccall foo24 , FunPtr (CInt -> Ptr (Ptr (Ptr CInt))) -> IO CInt
// #ccall foo25 , CInt -> IO (Ptr CInt)
// #ccall foo26 , CInt -> IO (Ptr (Ptr CInt))
// #ccall foo27 , CInt -> IO (Ptr (Ptr (Ptr CInt)))
// #ccall foo28 , CSize -> IO (Ptr (Ptr (Ptr CInt)))
// {- struct bar1_t {
//     void * a;
//     int b;
//     char c;
//     char * d;
//     char * (* e)(void);
//     void (* f)(void *);
//     int * (* g)(void *);
//     int * * (* h)(void *);
//     int * * * (* i)(void *);
//     char j[2];
//     struct bar1_t * k;
// }; -}
// #starttype struct bar1_t
// #field a , Ptr ()
// #field b , CInt
// #field c , CChar
// #field d , CString
// #field e , FunPtr CString
// #field f , FunPtr (Ptr () -> IO ())
// #field g , FunPtr (Ptr () -> Ptr CInt)
// #field h , FunPtr (Ptr () -> Ptr (Ptr CInt))
// #field i , FunPtr (Ptr () -> Ptr (Ptr (Ptr CInt)))
// #array_field j , CChar
// #field k , Ptr <struct bar1_t>
// #stoptype
// {- typedef struct bar2_t {
//             int a;
//         } bar2_t; -}
// #starttype struct bar2_t
// #field a , CInt
// #stoptype
// #synonym_t bar2_t , <struct bar2_t>
// {- typedef struct {
//             int a;
//         } bar3_t; -}
// #starttype bar3_t
// #field a , CInt
// #stoptype
// {- enum {
//     BAZ1 = 1
// }; -}
// #num BAZ1
// {- typedef enum {
//             BAZ2 = 1
//         } baz2_t; -}
// #integral_t baz2_t
// #num BAZ2
// {- enum baz3_t {
//     BAZ3 = 1
// }; -}
// #integral_t enum baz3_t
// #num BAZ3
// {- typedef enum baz4_t {
//             BAZ4 = 1
//         } baz4_t; -}
// #integral_t enum baz4_t
// #num BAZ4
// #synonym_t baz4_t , <enum baz4_t>
// #globalvar global , CInt
// #cinline inline_foo , CInt -> Ptr CInt -> CInt -> Ptr CInt -> Ptr (Ptr CInt) -> Ptr (Ptr CInt) -> CSize -> IO CInt
// #include <bindings.cmacros.h>

// BC_INLINE7(inline_foo, int, int*, const int, const int*, const int**, const int* const*, size_t, int)
// |]

// matches :: String -> String -> IO ()
// matches input output = do
//     res <- processString input
//     trim res `shouldBe` output

// tshow :: String -> Text
// tshow = pack . show

// trim :: String -> String
// trim = trimTail . dropWhile isSpace

// trimTail :: String -> String
// trimTail "" = ""
// trimTail s = take (lastNonBlank s) s
//   where lastNonBlank = (+1) . fst . foldl acc (0, 0)
//         acc (l, n) c | isSpace c = (l, n + 1)
//                      | otherwise = (n, n + 1)