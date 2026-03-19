/*******************************************************************************
 * Bitfield tests
 *
 * This header defines various structs with bit-fields as well as static
 * functions used to test them.
 *
 * Comments concerning alignment of fields in packed structs are valid for
 * objects at 64-bit aligned addresses, but note that hs-bindgen is able to
 * read/write bit-fields using only aligned peeks/pokes even when an object is
 * not aligned.
 *
 * In addition to golden tests, this header is tested using property tests in
 * test-th.  If any changes are made to this header, they must be reflected in
 * the TH tests.
 ******************************************************************************/

#ifndef TEST_BITFIELDS_H
#define TEST_BITFIELDS_H

#include <stdbool.h>

/*******************************************************************************
 * not packed, <=8-bit fields
 ******************************************************************************/
struct foo_8 {
  signed char a : 3;
  signed char b : 3;
  signed char c : 2;  // First byte fully packed
  signed char d : 3;  // Middle byte has padding
  signed char e : 8;  // 8-bit field
  signed char f : 5;  // Last byte has padding
};

static inline void set_foo_8(
    struct foo_8 *x,
    signed char a,
    signed char b,
    signed char c,
    signed char d,
    signed char e,
    signed char f) {
  x->a = a;
  x->b = b;
  x->c = c;
  x->d = d;
  x->e = e;
  x->f = f;
}

static inline bool eq_foo_8(
    struct foo_8 *x,
    signed char a,
    signed char b,
    signed char c,
    signed char d,
    signed char e,
    signed char f) {
  return
       x->a == a
    && x->b == b
    && x->c == c
    && x->d == d
    && x->e == e
    && x->f == f;
}

/*******************************************************************************
 * not packed, <=16-bit fields
 ******************************************************************************/
struct foo_16 {
  signed char a :  6;
  signed int  b : 10;  // First two bytes fully packed
  signed int  c : 16;  // 16-bit field
  signed int  d : 16;
  signed int  e : 12;  // Middle byte has padding
  signed int  f : 12;  // Padding at end
};

static inline void set_foo_16(
    struct foo_16 *x,
    signed char a,
    signed int  b,
    signed int  c,
    signed int  d,
    signed int  e,
    signed int  f) {
  x->a = a;
  x->b = b;
  x->c = c;
  x->d = d;
  x->e = e;
  x->f = f;
}

static inline bool eq_foo_16(
    struct foo_16 *x,
    signed char a,
    signed int  b,
    signed int  c,
    signed int  d,
    signed int  e,
    signed int  f) {
  return
       x->a == a
    && x->b == b
    && x->c == c
    && x->d == d
    && x->e == e
    && x->f == f;
}

/*******************************************************************************
 * not packed, <=32-bit fields
 ******************************************************************************/
struct foo_32 {
  signed char a :  6;
  signed int  b : 12;
  signed int  c : 14;  // First four bytes fully packed
  signed int  d : 10;  // Middle byte has padding
  signed long e : 32;  // 32-bit field
  signed int  f :  6;
  signed long g : 24;  // Padding at end
};

static inline void set_foo_32(
    struct foo_32 *x,
    signed char a,
    signed int  b,
    signed int  c,
    signed int  d,
    signed long e,
    signed int  f,
    signed long g) {
  x->a = a;
  x->b = b;
  x->c = c;
  x->d = d;
  x->e = e;
  x->f = f;
  x->g = g;
}

static inline bool eq_foo_32(
    struct foo_32 *x,
    signed char a,
    signed int  b,
    signed int  c,
    signed int  d,
    signed long e,
    signed int  f,
    signed long g) {
  return
       x->a == a
    && x->b == b
    && x->c == c
    && x->d == d
    && x->e == e
    && x->f == f
    && x->g == g;
}

/*******************************************************************************
 * not packed, <=64-bit fields
 ******************************************************************************/
struct foo_64 {
  signed long      a : 24;
  signed long long b : 40;  // First eight bytes fully packed
  signed long long c : 64;  // 64-bit field
  signed long long d : 36;  // Padding at end
};

static inline void set_foo_64(
    struct foo_64 *x,
    signed long      a,
    signed long long b,
    signed long long c,
    signed long long d) {
  x->a = a;
  x->b = b;
  x->c = c;
  x->d = d;
}

static inline bool eq_foo_64(
    struct foo_64 *x,
    signed long      a,
    signed long long b,
    signed long long c,
    signed long long d) {
  return
       x->a == a
    && x->b == b
    && x->c == c
    && x->d == d;
}

/*******************************************************************************
 * packed, <=8-bit field crosses 8-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_8_8 {
  signed char a : 6;
  signed int  b : 4;  // Crosses 8-bit word boundary
};

static inline void set_bar_8_8(
    struct bar_8_8 *x,
    signed char a,
    signed int  b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_8_8(
    struct bar_8_8 *x,
    signed char a,
    signed int  b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=8-bit field crosses 16-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_8_16 {
  signed int a : 14;
  signed int b :  4;  // Crosses 16-bit word boundary
};

static inline void set_bar_8_16(
    struct bar_8_16 *x,
    signed int a,
    signed int b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_8_16(
    struct bar_8_16 *x,
    signed int a,
    signed int b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=8-bit field crosses 32-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_8_32 {
  signed long a : 30;
  signed int  b :  4;  // Crosses 32-bit word boundary
};

static inline void set_bar_8_32(
    struct bar_8_32 *x,
    signed long a,
    signed int  b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_8_32(
    struct bar_8_32 *x,
    signed long a,
    signed int  b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=8-bit field crosses 64-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_8_64 {
  signed long long a : 62;
  signed int       b :  4;  // Crosses 64-bit word boundary
};

static inline void set_bar_8_64(
    struct bar_8_64 *x,
    signed long long a,
    signed int       b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_8_64(
    struct bar_8_64 *x,
    signed long long a,
    signed int       b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=16-bit field crosses 16-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_16_16 {
  signed int a : 14;
  signed int b : 14;  // Crosses 16-bit word boundary
};

static inline void set_bar_16_16(
    struct bar_16_16 *x,
    signed int a,
    signed int b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_16_16(
    struct bar_16_16 *x,
    signed int a,
    signed int b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=16-bit field crosses 32-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_16_32 {
  signed long a : 24;
  signed int  b : 14;  // Crosses 32-bit word boundary
};

static inline void set_bar_16_32(
    struct bar_16_32 *x,
    signed long a,
    signed int  b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_16_32(
    struct bar_16_32 *x,
    signed long a,
    signed int  b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=16-bit field crosses 64-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_16_64 {
  signed long long a : 56;
  signed int       b : 14;  // Crosses 64-bit word boundary
};

static inline void set_bar_16_64(
    struct bar_16_64 *x,
    signed long long a,
    signed int       b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_16_64(
    struct bar_16_64 *x,
    signed long long a,
    signed int       b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=32-bit field crosses 32-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_32_32 {
  signed long a : 30;
  signed long b : 30;  // Crosses 32-bit word boundary
};

static inline void set_bar_32_32(
    struct bar_32_32 *x,
    signed long a,
    signed long b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_32_32(
    struct bar_32_32 *x,
    signed long a,
    signed long b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=32-bit field crosses 64-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_32_64 {
  signed long long a : 56;
  signed long      b : 30;  // Crosses 64-bit word boundary
};

static inline void set_bar_32_64(
    struct bar_32_64 *x,
    signed long long a,
    signed long      b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_32_64(
    struct bar_32_64 *x,
    signed long long a,
    signed long      b) {
  return
       x->a == a
    && x->b == b;
}

/*******************************************************************************
 * packed, <=64-bit field crosses 64-bit word boundary
 ******************************************************************************/
struct __attribute__((packed)) bar_64_64 {
  signed long long a : 56;
  signed long long b : 40;  // Crosses 64-bit word boundary
};

static inline void set_bar_64_64(
    struct bar_64_64 *x,
    signed long long a,
    signed long long b) {
  x->a = a;
  x->b = b;
}

static inline bool eq_bar_64_64(
    struct bar_64_64 *x,
    signed long long a,
    signed long long b) {
  return
       x->a == a
    && x->b == b;
}

#endif // TEST_BITFIELDS_H
