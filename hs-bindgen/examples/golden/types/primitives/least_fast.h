// These types differ across libc implementation, not just platform.  We are
// therefore unable to provide Haskell types for them in the hs-bindgen
// standard library.

#include <stdint.h>

int_least8_t  int_least8_t_fun();
int_least16_t int_least16_t_fun();
int_least32_t int_least32_t_fun();
int_least64_t int_least64_t_fun();

uint_least8_t  uint_least8_t_fun();
uint_least16_t uint_least16_t_fun();
uint_least32_t uint_least32_t_fun();
uint_least64_t uint_least64_t_fun();

int_fast8_t  int_fast8_t_fun();
int_fast16_t int_fast16_t_fun();
int_fast32_t int_fast32_t_fun();
int_fast64_t int_fast64_t_fun();

uint_fast8_t  uint_fast8_t_fun();
uint_fast16_t uint_fast16_t_fun();
uint_fast32_t uint_fast32_t_fun();
uint_fast64_t uint_fast64_t_fun();
