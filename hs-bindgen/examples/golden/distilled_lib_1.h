/**
 * Distillation of some test cases from a proprietary C library
 */

/* amendmends */
#include <stdbool.h>
typedef unsigned char  uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int   uint32_t;
typedef signed   int   int32_t;

typedef struct { int foo; char bar; } another_typedef_struct_t;
typedef enum { FOO, BAR } another_typedef_enum_e;
#define A 5
#define B 3
#define SOME_DEFINED_CONSTANT 4
typedef int a_type_t;
typedef int var_t;

#ifdef __GNUC__
#define PACK_START     _Pragma("pack(1)")
#define PACK_FINISH    _Pragma("pack()")
#elif
#define PACK_START
#define PACK_FINISH
#endif

#ifdef __GNUC__
#define PACK_ENUM __attribute__((packed))
#else
#define PACK_ENUM
#endif

// Structs

/* Some comment*/
PACK_START
typedef struct a_typedef_struct
{
    bool                      field_0;    /* field 0 comment */
    uint8_t                   field_1;    /* field 1 comment */
    uint16_t                  field_2;    /* field 2 comment */
    uint32_t                  field_3;    /* field 3 comment */
    another_typedef_struct_t  field_4;    /* struct field */
    another_typedef_struct_t *field_5;    /* struct pointer */
    void                     *field_6;    /* void pointer */
    uint32_t                  field_7[7]; /* an array field */
    another_typedef_enum_e    field_8;    /* an enum field */
    another_typedef_enum_e    field_9[SOME_DEFINED_CONSTANT]; /* an enum array field */
    another_typedef_enum_e    field_10[A][B]; /* multi-dim array field */
} a_typedef_struct_t;
PACK_FINISH

// Defines

#define A_DEFINE_0 0x00 /* a comment */
#define A_DEFINE_1 (0x5050U)
#define A_DEFINE_2 2
#define TWO_ARGS   0x3456, 0x789A

// Enums

/* A comment */
typedef enum
{
  ENUM_CASE_0 = 0,
  ENUM_CASE_1 = 1u,
  ENUM_CASE_2 = 2U,
  ENUM_CASE_3 = 0x3,
} PACK_ENUM a_typedef_enum_e;

// Functions

/* some comment */
int32_t some_fun(a_type_t *i, uint32_t j, uint8_t k[]);

// Callbacks

/* a comment */
typedef uint32_t(*callback_t)(void *p, uint32_t k);

// Global variables
//
// Perhaps this variable is declared in a .c file somewhere as:
//
// ```
// var_t v =
//   { .field1 = {.nested_field = 0},
//     .field2 = false,
//     .field3 = {0, 0, 0, 0},
//   };
// ```

extern var_t v;
