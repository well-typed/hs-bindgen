/** @file
 * Examples from the c2hsc test suite
 *
 * https://github.com/jwiegley/c2hsc/blob/master/test/main.hs
 */

/**
 * Issues
 */

// typedef const is seen as const rather than typedef (#38)
typedef const char* an_pchar;

// incorrect code generated for char x[8][255" (#25)
typedef struct {
    char listOfNames[8][255];
} MyCoolStruct;

// Export function pointers as callbacks rather than just synonyms (#17)
typedef int (*foo)(int);

// function pointer arrays
//
// NOTE: This test is commented out in the original test suite.
// NOTE: Modified it to be 'extern' (to make the incomplete array possible).
extern int (*my_array[])(int);

// function pointer structure members
//
// NOTE: This test is commented out in the original test suite.
struct foo_t {
    int (*foo_member)(int);
};

// function pointer function arguments
//
// NOTE: This test is commented out in the original test suite.
void foo_function(int (*foo)(int)) {}

// Enum type names are sometimes handled incorrectly (#10)
//
// NOTE: Test suite refers to #15 here, but I think that's a mistake.
// NOTE: Renamed to avoid conflicts.
typedef struct Foo_10_ Foo_10;
typedef enum Bar_10_ { BAR } Bar_10;

// Non-typedefed structs, unions, and enums are not correctly translated (#12)
struct st {
  int i;
};

enum e {
  CONST
};

union u {
  char c;
};

// Duplicate opaque_t struct (#15)
struct MyTypeImpl;
typedef struct MyTypeImpl* MyType;

typedef struct MyStruct {
  int x;
} MyStructType;

typedef struct MyStructEmpty MyStructEmptyType;

// "unsigned" as a type is being translated to unit (#33)
//
// NOTE: Renamed to avoid conflicts.
unsigned foo_33_1(unsigned);
unsigned __attribute__ ((visibility ("default"))) foo_33_2(unsigned);
long long __attribute__ ((visibility ("default"))) foo_33_3(long long);

/**
 * Primitive types
 *
 * NOTE: Here and elsewhere the original test suite has some references to `long
 * double`, some commented out, and some---confusingly---not, with an
 * expectation of a translation to `CDouble`; the latter I think is a bug in the
 * original test suite. Here we leave all of these test cases in; we emit an
 * "unsupported" message for these and omit the declaration in the generated
 * bindings.
 */

float              ordinary_float;
double             ordinary_double;
long double        ordinary_long_double;

char               ordinary_char;
signed char        signed_char;
unsigned char      unsigned_char;

short              ordinary_signed_short;
signed short       explicit_signed_short;
unsigned short     unsigned_short;

int                ordinary_signed_int;
signed int         explicit_signed_int;
unsigned int       unsigned_int;

long               ordinary_signed_long;
signed long        explicit_signed_long;
unsigned long      unsigned_long;

long long          ordinary_signed_long_long;
signed long long   explicit_signed_long_long;
unsigned long long unsigned_long_long;

/**
 * Pointers: primitive types which cannot be signed
 */

void*               ordinary_void_pointer;

float*              ordinary_float_pointer;
double*             ordinary_double_pointer;
long double*        ordinary_long_double_pointer;

char*               ordinary_char_pointer;
signed char*        signed_char_pointer;
unsigned char*      unsigned_char_pointer;

short*              ordinary_signed_short_pointer;
signed short*       explicit_signed_short_pointer;
unsigned short*     unsigned_short_pointer;

int*                ordinary_signed_int_pointer;
signed int*         explicit_signed_int_pointer;
unsigned int*       unsigned_int_pointer;

long*               ordinary_signed_long_pointer;
signed long*        explicit_signed_long_pointer;
unsigned long*      unsigned_long_pointer;

long long*          ordinary_signed_long_long_pointer;
signed long long*   explicit_signed_long_long_pointer;
unsigned long long* unsigned_long_long_pointer;

/**
 * Arrays: primitive types which cannot be signed
 */

float              ordinary_float_array[10];
double             ordinary_double_array[10];
long double        ordinary_long_double_array[10];

char               ordinary_signed_char_array[10];
signed char        explicit_signed_char_array[10];
unsigned char      unsigned_char_array[10];

short              ordinary_signed_short_array[10];
signed short       explicit_signed_short_array[10];
unsigned short     unsigned_short_array[10];

int                ordinary_signed_int_array[10];
signed int         explicit_signed_int_array[10];
unsigned int       unsigned_int_array[10];

long               ordinary_signed_long_array[10];
signed long        explicit_signed_long_array[10];
unsigned long      unsigned_long_array[10];

long long          ordinary_signed_long_long_array[10];
signed long long   explicit_signed_long_long_array[10];
unsigned long long unsigned_long_long_array[10];

/**
 * Arrays of pointers
 */

void*               ordinary_void_pointer_array[10];

float*              ordinary_float_pointer_array[10];
double*             ordinary_double_pointer_array[10];
long double*        ordinary_long_double_pointer_array[10];

char*               ordinary_signed_char_pointer_array[10];
signed char*        explicit_signed_char_pointer_array[10];
unsigned char*      unsigned_char_pointer_array[10];

short*              ordinary_signed_short_pointer_array[10];
signed short*       explicit_signed_short_pointer_array[10];
unsigned short*     unsigned_short_pointer_array[10];

int*                ordinary_signed_int_pointer_array[10];
signed int*         explicit_signed_int_pointer_array[10];
unsigned int*       unsigned_int_pointer_array[10];

long*               ordinary_signed_long_pointer_array[10];
signed long*        explicit_signed_long_pointer_array[10];
unsigned long*      unsigned_long_pointer_array[10];

long long*          ordinary_signed_long_long_pointer_array[10];
signed long long*   explicit_signed_long_long_pointer_array[10];
unsigned long long* unsigned_long_long_pointer_array[10];

/**
 * Structs: primitive types
 */

struct ordinary_float_struct            { float              ordinary_float_member;            };
struct ordinary_double_struct           { double             ordinary_double_member;           };
struct ordinary_long_double_struct      { long double        ordinary_long_double_member;      };

struct ordinary_signed_char_struct      { char               ordinary_signed_char_member;      };
struct explicit_signed_char_struct      { signed char        explicit_signed_char_member;      };
struct unsigned_char_struct             { unsigned char      unsigned_char_member;             };

struct ordinary_signed_short_struct     { short              ordinary_signed_short_member;     };
struct explicit_signed_short_struct     { signed short       explicit_signed_short_member;     };
struct unsigned_short_struct            { unsigned short     unsigned_short_member;            };

struct ordinary_signed_int_struct       { int                ordinary_signed_int_member;       };
struct explicit_signed_int_struct       { signed int         explicit_signed_int_member;       };
struct unsigned_int_struct              { unsigned int       unsigned_int_member;              };

struct ordinary_signed_long_struct      { long               ordinary_signed_long_member;      };
struct explicit_signed_long_struct      { signed long        explicit_signed_long_member;      };
struct unsigned_long_struct             { unsigned long      unsigned_long_member;             };

struct ordinary_signed_long_long_struct { long long          ordinary_signed_long_long_member; };
struct explicit_signed_long_long_struct { signed long long   explicit_signed_long_long_member; };
struct unsigned_long_long_struct        { unsigned long long unsigned_long_long_member;        };

/**
 * Structs: pointers
 *
 * NOTE: `ordinary_signed_char_pointer_struct` is commented out in the original
 * test suite, unclear why (no reason is given).
 */

struct ordinary_void_pointer_struct             { void*               ordinary_void_pointer_member;             };

struct ordinary_float_pointer_struct            { float*              ordinary_float_pointer_member;            };
struct ordinary_double_pointer_struct           { double*             ordinary_double_pointer_member;           };
struct ordinary_long_double_pointer_struct      { long double*        ordinary_long_double_pointer_member;      };

struct ordinary_signed_char_pointer_struct      { char*               ordinary_signed_char_pointer_member;      };
struct explicit_signed_char_pointer_struct      { signed char*        explicit_signed_char_pointer_member;      };
struct unsigned_char_pointer_struct             { unsigned char*      unsigned_char_pointer_member;             };

struct ordinary_signed_short_pointer_struct     { short*              ordinary_signed_short_pointer_member;     };
struct explicit_signed_short_pointer_struct     { signed short*       explicit_signed_short_pointer_member;     };
struct unsigned_short_pointer_struct            { unsigned short*     unsigned_short_pointer_member;            };

struct ordinary_signed_int_pointer_struct       { int*                ordinary_signed_int_pointer_member;       };
struct explicit_signed_int_pointer_struct       { signed int*         explicit_signed_int_pointer_member;       };
struct unsigned_int_pointer_struct              { unsigned int*       unsigned_int_pointer_member;              };

struct ordinary_signed_long_pointer_struct      { long*               ordinary_signed_long_pointer_member;      };
struct explicit_signed_long_pointer_struct      { signed long*        explicit_signed_long_pointer_member;      };
struct unsigned_long_pointer_struct             { unsigned long*      unsigned_long_pointer_member;             };

struct ordinary_signed_long_long_pointer_struct { long long*          ordinary_signed_long_long_pointer_member; };
struct explicit_signed_long_long_pointer_struct { signed long long*   explicit_signed_long_long_pointer_member; };
struct unsigned_long_long_pointer_struct        { unsigned long long* unsigned_long_long_pointer_member;        };

/**
 * Structs: arrays
 */

struct ordinary_float_array_struct            { float              ordinary_float_array_member[10];            };
struct ordinary_double_array_struct           { double             ordinary_double_array_member[10];           };
struct ordinary_long_double_array_struct      { long double        ordinary_long_double_array_member[10];      };

struct ordinary_signed_char_array_struct      { char               ordinary_signed_char_array_member[10];      };
struct explicit_signed_char_array_struct      { signed char        explicit_signed_char_array_member[10];      };
struct unsigned_char_array_struct             { unsigned char      unsigned_char_array_member[10];             };

struct ordinary_signed_short_array_struct     { short              ordinary_signed_short_array_member[10];     };
struct explicit_signed_short_array_struct     { signed short       explicit_signed_short_array_member[10];     };
struct unsigned_short_array_struct            { unsigned short     unsigned_short_array_member[10];            };

struct ordinary_signed_int_array_struct       { int                ordinary_signed_int_array_member[10];       };
struct explicit_signed_int_array_struct       { signed int         explicit_signed_int_array_member[10];       };
struct unsigned_int_array_struct              { unsigned int       unsigned_int_array_member[10];              };

struct ordinary_signed_long_array_struct      { long               ordinary_signed_long_array_member[10];      };
struct explicit_signed_long_array_struct      { signed long        explicit_signed_long_array_member[10];      };
struct unsigned_long_array_struct             { unsigned long      unsigned_long_array_member[10];             };

struct ordinary_signed_long_long_array_struct { long long          ordinary_signed_long_long_array_member[10]; };
struct explicit_signed_long_long_array_struct { signed long long   explicit_signed_long_long_array_member[10]; };
struct unsigned_long_long_array_struct        { unsigned long long unsigned_long_long_array_member[10];        };

/**
 * Structs: arrays of pointers
 *
 * NOTE: Here too `ordinary_signed_char_pointer_array_struct` was commented out
 * in the original test suite, with no reason given.
 */

struct ordinary_void_pointer_array_struct             { void*               ordinary_void_pointer_array_member[10];             };

struct ordinary_float_pointer_array_struct            { float*              ordinary_float_pointer_array_member[10];            };
struct ordinary_double_pointer_array_struct           { double*             ordinary_double_pointer_array_member[10];           };
struct ordinary_long_double_pointer_array_struct      { long double*        ordinary_long_double_pointer_array_member[10];      };

struct ordinary_signed_char_pointer_array_struct      { char*               ordinary_signed_char_pointer_array_member[10];      };
struct explicit_signed_char_pointer_array_struct      { signed char*        explicit_signed_char_pointer_array_member[10];      };
struct unsigned_char_pointer_array_struct             { unsigned char*      unsigned_char_pointer_array_member[10];             };

struct ordinary_signed_short_pointer_array_struct     { short*              ordinary_signed_short_pointer_array_member[10];     };
struct explicit_signed_short_pointer_array_struct     { signed short*       explicit_signed_short_pointer_array_member[10];     };
struct unsigned_short_pointer_array_struct            { unsigned short*     unsigned_short_pointer_array_member[10];            };

struct ordinary_signed_int_pointer_array_struct       { int*                ordinary_signed_int_pointer_array_member[10];       };
struct explicit_signed_int_pointer_array_struct       { signed int*         explicit_signed_int_pointer_array_member[10];       };
struct unsigned_int_pointer_array_struct              { unsigned int*       unsigned_int_pointer_array_member[10];              };

struct ordinary_signed_long_pointer_array_struct      { long*               ordinary_signed_long_pointer_array_member[10];      };
struct explicit_signed_long_pointer_array_struct      { signed long*        explicit_signed_long_pointer_array_member[10];      };
struct unsigned_long_pointer_array_struct             { unsigned long*      unsigned_long_pointer_array_member[10];             };

struct ordinary_signed_long_long_pointer_array_struct { long long*          ordinary_signed_long_long_pointer_array_member[10]; };
struct explicit_signed_long_long_pointer_array_struct { signed long long*   explicit_signed_long_long_pointer_array_member[10]; };
struct unsigned_long_long_pointer_array_struct        { unsigned long long* unsigned_long_long_pointer_array_member[10];        };

/**
 * Sanity checks
 *
 * NOTE: The `smoke.h` test is moved to a separate header.
 */

// maps a typedef
typedef int an_int;

/**
 * Issues without test cases in the original test suite
 *
 * These are examples from open issues on the c2hsc repository, that don't (yet)
 * have corresponding test cases in the c2hsc test suite.
 */

// Arrays of anonymous structs need a generated name (#21)
struct cal_table {
    int size;
    struct {
        int raw; int val;
    } table[32];
};

// Anonymous unions within structs seem to not work (#23)
typedef struct {
  unsigned char d_tag[4];   /* entry tag value */
  union {
    unsigned char d_val[4];
    unsigned char d_ptr[4];
  } d_un;
} Elf32_External_Dyn;

// pointer typedef generates incorrect type (#24)
//
// NOTE: Changed the type in the second example to avoid dependencies.
typedef int * bug_24;
typedef const int  *bug_24_2;

// support top-level array typedefs (#27)
//
// NOTE: Renamed to avoid conflicts.
typedef int MyArray_27[20];
typedef struct {
  MyArray_27 x;
} MyStruct_27;
