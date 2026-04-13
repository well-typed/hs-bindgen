#include <stddef.h>

#define A int

// Preliminaries

struct some_struct {};
union some_union {};
enum some_enum { ENUM_A };

/**
 * Function declarations
 */

// Arguments

void args_char1    (A arg1,          char arg2);
void args_char2    (A arg1,   signed char arg2);
void args_char3    (A arg1, unsigned char arg2);

void args_short1   (A arg1,          short arg2);
void args_short2   (A arg1,   signed short arg2);
void args_short3   (A arg1, unsigned short arg2);

void args_int1     (A arg1,          int arg2);
void args_int2     (A arg1,   signed int arg2);
void args_int3     (A arg1, unsigned int arg2);

void args_long1    (A arg1,          long arg2);
void args_long2    (A arg1,   signed long arg2);
void args_long3    (A arg1, unsigned long arg2);

void args_float    (A arg1, float  arg2);
void args_double   (A arg1, double arg2);
void args_bool1    (A arg1, _Bool  arg2);

void args_struct   (A arg1, struct some_struct arg2);
void args_union    (A arg1, union  some_union  arg2);
void args_enum     (A arg1, enum   some_enum   arg2);

void args_pointer1 (A arg1, int  *  arg2);
void args_pointer2 (A arg1, int  ** arg2);
void args_pointer3 (A arg1, void *  arg3);

// Different return types

A ret_A();

         char      ret_char1    (A arg1);
  signed char      ret_char2    (A arg1);
unsigned char      ret_char3    (A arg1);

         short     ret_short1   (A arg1);
  signed short     ret_short2   (A arg1);
unsigned short     ret_short3   (A arg1);

         int       ret_int1     (A arg1);
  signed int       ret_int2     (A arg1);
unsigned int       ret_int3     (A arg1);

         long      ret_long1    (A arg1);
  signed long      ret_long2    (A arg1);
unsigned long      ret_long3    (A arg1);

float              ret_float    (A arg1);
double             ret_double   (A arg1);
_Bool              ret_bool1    (A arg1);

struct some_struct ret_struct   (A arg1);
union  some_union  ret_union    (A arg1);
enum   some_enum   ret_enum     (A arg1);

int  *             ret_pointer1 (A arg1);
int  **            ret_pointer2 (A arg1);
void *             ret_pointer3 (A arg1);

// With function body

int body1(A arg1) { return arg1; }
A body2() { return 0; }

// Complex numbers

void args_complex_float  (A arg1, float  _Complex arg2);
void args_complex_double (A arg1, double _Complex arg2);
float  _Complex ret_complex_float  (A arg1);
double _Complex ret_complex_double (A arg1);

// Bespoke types
//
// These are types that the language_c parser does not recognize; we add them
// as explicit entries into the type environment.

void bespoke_args1 (A arg1, bool   arg2);
void bespoke_args2 (A arg1, size_t arg2);

bool   bespoke_ret1 (A arg1);
size_t bespoke_ret2 (A arg1);

/**
 * Arrays
 */

void arr_args1(A  arg1[]);
void arr_args2(A* arg1[]);
void arr_args3(A  arg1[5]);
void arr_args4(A* arg1[5]);

typedef A   arr_typedef1[];
typedef A*  arr_typedef2[];
typedef A   arr_typedef3[5];
typedef A*  arr_typedef4[5];

/**
 * Typedefs
 */

typedef A    typedef1;
typedef A *  typedef2;
typedef A ** typedef3;

/**
 * Function pointers
 */

void funptr_args1 (A arg1,  void (*arg2)());
void funptr_args2 (A arg1,  int  (*arg2)());
void funptr_args3 (A arg1,  void (*arg2)(int));
void funptr_args4 (A arg1,  char (*arg2)(int, double));
void funptr_args5 (A arg1,  int* (*arg2)(int, double));

typedef A    (*funptr_typedef1)();
typedef A *  (*funptr_typedef2)();
typedef A ** (*funptr_typedef3)();
typedef A    (*funptr_typedef4)(int, double);
typedef A *  (*funptr_typedef5)(int, double);

/**
 * Comments in awkward places
 *
 * (Prior to language-c we failed to parse there.)
 */

void /* some comment */ comments1(A arg1);
typedef A /* some comment */ comments2;

/**
 * Struct fields
 */

struct example_struct {
  A    field1;
  A *  field2;
  A ** field3;
};

/**
 * TODO: Globals and constants
 *
 * https://github.com/well-typed/hs-bindgen/issues/831
 */

// extern A    global1;
// extern A *  global2;
// extern A ** global3;

// extern A    const const1;
// extern A *  const const2;
// extern A ** const const3;

/**
 * `const` qualifier
 *
 * NOTE: These were not parsed correctly prior to the switch to language-c.
 */

// Types with a sign

void const_prim_before1 (A arg1, const          char arg2);
void const_prim_before2 (A arg1, const   signed char arg2);
void const_prim_before3 (A arg1, const unsigned char arg2);
void const_prim_after1  (A arg1,          char const arg2);
void const_prim_after2  (A arg1,   signed char const arg2);
void const_prim_after3  (A arg1, unsigned char const arg2);

// Types without a sign

void const_withoutSign_before1 (A arg1, const float              arg2);
void const_withoutSign_before2 (A arg1, const double             arg2);
void const_withoutSign_before3 (A arg1, const _Bool              arg2);
void const_withoutSign_before4 (A arg1, const struct some_struct arg2);
void const_withoutSign_before5 (A arg1, const union  some_union  arg2);
void const_withoutSign_before6 (A arg1, const enum   some_enum   arg2);
void const_withoutSign_before7 (A arg1, const bool               arg2);
void const_withoutSign_before8 (A arg1, const size_t             arg2);

void const_withoutSign_after1 (A arg1, float              const arg2);
void const_withoutSign_after2 (A arg1, double             const arg2);
void const_withoutSign_after3 (A arg1, _Bool              const arg2);
void const_withoutSign_after4 (A arg1, struct some_struct const arg2);
void const_withoutSign_after5 (A arg1, union  some_union  const arg2);
void const_withoutSign_after6 (A arg1, enum   some_enum   const arg2);
void const_withoutSign_after7 (A arg1, bool               const arg2);
void const_withoutSign_after8 (A arg1, size_t             const arg2);

// Pointers

void const_pointers_args1 (A arg1, const int * arg2);
void const_pointers_args2 (A arg1, int const * arg2);
void const_pointers_args3 (A arg1, int * const arg2);
void const_pointers_args4 (A arg1, const int * const arg2);
void const_pointers_args5 (A arg1, int const * const arg2);

const int *       const_pointers_ret1 (A arg1);
int const *       const_pointers_ret2 (A arg1);
int * const       const_pointers_ret3 (A arg1);
const int * const const_pointers_ret4 (A arg1);
int const * const const_pointers_ret5 (A arg1);

typedef const A         const_typedef1;
typedef A const         const_typedef2;
typedef const A *       const_typedef3;
typedef A const *       const_typedef4;
typedef A * const       const_typedef5;
typedef const A * const const_typedef6;
typedef A const * const const_typedef7;

struct example_struct_with_const {
  const A         const_field1;
  A const         const_field2;
  const A *       const_field3;
  A const *       const_field4;
  A * const       const_field5;
  const A * const const_field6;
  A const * const const_field7;
};

typedef const A         (*const_funptr1)(int, double);
typedef A const         (*const_funptr2)(int, double);
typedef const A *       (*const_funptr3)(int, double);
typedef A const *       (*const_funptr4)(int, double);
typedef A * const       (*const_funptr5)(int, double);
typedef const A * const (*const_funptr6)(int, double);
typedef A const * const (*const_funptr7)(int, double);

void const_array_elem1(A const   arg1[]);
void const_array_elem2(A const * arg1[]);
void const_array_elem3(A * const arg1[]);

/**
 * Other examples we reparsed /incorrectly/ before language-c
 */

// Explicitly empty parameter list

A noParams1(void);
A noParams2(void) { return 0; }
void noParams3(A arg1, int (*arg2)(void));

// Returning function pointers

void (*funptr_ret1(A arg1)) ();
int  (*funptr_ret2(A arg1)) ();
void (*funptr_ret3(A arg1)) (int);
char (*funptr_ret4(A arg1)) (int, double);

// Returning function pointers to functions that themselves return pointers

int*             (*funptr_ret5(A arg1))  (int, double);
const int*       (*funptr_ret6(A arg1))  (int, double);
int const*       (*funptr_ret7(A arg1))  (int, double);
int* const       (*funptr_ret8(A arg1))  (int, double);
const int* const (*funptr_ret9(A arg1))  (int, double);
int const* const (*funptr_ret10(A arg1)) (int, double);

/**
 * Macro-defined types
 */

#define BOOL  _Bool
#define INT   int
#define INTP  int*
#define INTCP const int* const
