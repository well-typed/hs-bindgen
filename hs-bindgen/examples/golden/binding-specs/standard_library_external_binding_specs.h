/* TODO: https://github.com/well-typed/hs-bindgen/issues/1690. Automatic test. */

/* Boolean types */
#include <stdbool.h>
bool bool_fun();

/* Integral types */
#include <stdint.h>
int8_t int8_t_fun();
int16_t int16_t_fun();
int32_t int32_t_fun();
int64_t int64_t_fun();
uint8_t uint8_t_fun();
uint16_t uint16_t_fun();
uint32_t uint32_t_fun();
uint64_t uint64_t_fun();
intmax_t intmax_t_fun();
uintmax_t uintmax_t_fun();
intptr_t intptr_t_fun();
uintptr_t uintptr_t_fun();

/* Floating types */
#include <fenv.h>
fenv_t *fenv_t_fun();
fexcept_t *fexcept_t_fun();

/* Standard types */
#include <stddef.h>
size_t size_t_fun();
ptrdiff_t ptrdiff_t_fun();

/* Non-local jump types */
#include <setjmp.h>
/* Functions cannot return array types, so we use a value for `jmp_buf`. */
extern jmp_buf jmp_buf_val;

/* Wide character types */
#include <uchar.h>
#include <wchar.h>
#include <wctype.h>
wchar_t wchar_t_fun();
wint_t wint_t_fun();
mbstate_t *mbstate_t_fun();
wctrans_t wctrans_t_fun();
wctype_t wctype_t_fun();
char16_t char16_t_fun();
char32_t char32_t_fun();

/* Time types */
#include <time.h>
time_t time_t_fun();
clock_t clock_t_fun();
/* TODO: https://github.com/well-typed/hs-bindgen/issues/786 */
/* struct tm struct_tm_fun(); */

/* File types */
#include <stdio.h>
FILE *FILE_fun();
fpos_t *fpos_t_fun();

/* Signal types */
#include <signal.h>
sig_atomic_t sig_atomic_t_fun();
