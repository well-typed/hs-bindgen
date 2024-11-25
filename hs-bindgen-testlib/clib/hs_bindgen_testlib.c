#include <math.h>
#include <signal.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>

#include "hs_bindgen_testlib.h"

/*******************************************************************************
  Storable: sizeof
*******************************************************************************/

size_t hsbg_sizeof_CChar(void) {
  return sizeof (char);
}

size_t hsbg_sizeof_CSChar(void) {
  return sizeof (signed char);
}

size_t hsbg_sizeof_CUChar(void) {
  return sizeof (unsigned char);
}

size_t hsbg_sizeof_CShort(void) {
  return sizeof (short);
}

size_t hsbg_sizeof_CUShort(void) {
  return sizeof (unsigned short);
}

size_t hsbg_sizeof_CInt(void) {
  return sizeof (int);
}

size_t hsbg_sizeof_CUInt(void) {
  return sizeof (unsigned int);
}

size_t hsbg_sizeof_CLong(void) {
  return sizeof (long);
}

size_t hsbg_sizeof_CULong(void) {
  return sizeof (unsigned long);
}

size_t hsbg_sizeof_CPtrdiff(void) {
  return sizeof (ptrdiff_t);
}

size_t hsbg_sizeof_CSize(void) {
  return sizeof (size_t);
}

size_t hsbg_sizeof_CWchar(void) {
  return sizeof (wchar_t);
}

size_t hsbg_sizeof_CSigAtomic(void) {
  return sizeof (sig_atomic_t);
}

size_t hsbg_sizeof_CLLong(void) {
  return sizeof (long long);
}

size_t hsbg_sizeof_CULLong(void) {
  return sizeof (unsigned long long);
}

size_t hsbg_sizeof_CBool(void) {
  return sizeof (bool);
}

size_t hsbg_sizeof_CIntPtr(void) {
  return sizeof (intptr_t);
}

size_t hsbg_sizeof_CUIntPtr(void) {
  return sizeof (uintptr_t);
}

size_t hsbg_sizeof_CIntMax(void) {
  return sizeof (intmax_t);
}

size_t hsbg_sizeof_CUIntMax(void) {
  return sizeof (uintmax_t);
}

size_t hsbg_sizeof_CClock(void) {
  return sizeof (clock_t);
}

size_t hsbg_sizeof_CTime(void) {
  return sizeof (time_t);
}

size_t hsbg_sizeof_CFloat(void) {
  return sizeof (float);
}

size_t hsbg_sizeof_CDouble(void) {
  return sizeof (double);
}

/*******************************************************************************
  Storable: alignof
*******************************************************************************/

size_t hsbg_alignof_CChar(void) {
  return alignof(char);
}

size_t hsbg_alignof_CSChar(void) {
  return alignof(signed char);
}

size_t hsbg_alignof_CUChar(void) {
  return alignof(unsigned char);
}

size_t hsbg_alignof_CShort(void) {
  return alignof(short);
}

size_t hsbg_alignof_CUShort(void) {
  return alignof(unsigned short);
}

size_t hsbg_alignof_CInt(void) {
  return alignof(int);
}

size_t hsbg_alignof_CUInt(void) {
  return alignof(unsigned int);
}

size_t hsbg_alignof_CLong(void) {
  return alignof(long);
}

size_t hsbg_alignof_CULong(void) {
  return alignof(unsigned long);
}

size_t hsbg_alignof_CPtrdiff(void) {
  return alignof(ptrdiff_t);
}

size_t hsbg_alignof_CSize(void) {
  return alignof(size_t);
}

size_t hsbg_alignof_CWchar(void) {
  return alignof(wchar_t);
}

size_t hsbg_alignof_CSigAtomic(void) {
  return alignof(sig_atomic_t);
}

size_t hsbg_alignof_CLLong(void) {
  return alignof(long long);
}

size_t hsbg_alignof_CULLong(void) {
  return alignof(unsigned long long);
}

size_t hsbg_alignof_CBool(void) {
  return alignof(bool);
}

size_t hsbg_alignof_CIntPtr(void) {
  return alignof(intptr_t);
}

size_t hsbg_alignof_CUIntPtr(void) {
  return alignof(uintptr_t);
}

size_t hsbg_alignof_CIntMax(void) {
  return alignof(intmax_t);
}

size_t hsbg_alignof_CUIntMax(void) {
  return alignof(uintmax_t);
}

size_t hsbg_alignof_CClock(void) {
  return alignof(clock_t);
}

size_t hsbg_alignof_CTime(void) {
  return alignof(time_t);
}

size_t hsbg_alignof_CFloat(void) {
  return alignof(float);
}

size_t hsbg_alignof_CDouble(void) {
  return alignof(double);
}

/*******************************************************************************
  Transform
*******************************************************************************/

char hsbg_transform_CChar(char c) {
  return c + 1;
}

signed char hsbg_transform_CSChar(signed char c) {
  return c + 1;
}

unsigned char hsbg_transform_CUChar(unsigned char c) {
  return c + 1;
}

short hsbg_transform_CShort(short n) {
  return n + 1;
}

unsigned short hsbg_transform_CUShort(unsigned short n) {
  return n + 1;
}

int hsbg_transform_CInt(int n) {
  return n + 1;
}

unsigned int hsbg_transform_CUInt(unsigned int n) {
  return n + 1;
}

long hsbg_transform_CLong(long n) {
  return n + 1;
}

unsigned long hsbg_transform_CULong(unsigned long n) {
  return n + 1;
}

ptrdiff_t hsbg_transform_CPtrdiff(ptrdiff_t n) {
  return n + 1;
}

size_t hsbg_transform_CSize(size_t n) {
  return n + 1;
}

wchar_t hsbg_transform_CWchar(wchar_t c) {
  return c + 1;
}

sig_atomic_t hsbg_transform_CSigAtomic(sig_atomic_t n) {
  return n + 1;
}

long long hsbg_transform_CLLong(long long n) {
  return n + 1;
}

unsigned long long hsbg_transform_CULLong(unsigned long long n) {
  return n + 1;
}

bool hsbg_transform_CBool(bool b) {
  return !b;
}

intptr_t hsbg_transform_CIntPtr(intptr_t n) {
  return n + 1;
}

uintptr_t hsbg_transform_CUIntPtr(uintptr_t n) {
  return n + 1;
}

intmax_t hsbg_transform_CIntMax(intmax_t n) {
  return n + 1;
}

uintmax_t hsbg_transform_CUIntMax(uintmax_t n) {
  return n + 1;
}

clock_t hsbg_transform_CClock(clock_t c) {
  return c + 1;
}

time_t hsbg_transform_CTime(time_t t) {
  return t + 1;
}

float hsbg_transform_CFloat(float x) {
  if (isnan(x))               return -0.0;
  if (x == 0.0 && signbit(x)) return NAN;
  if (isinf(x))               return -x;

  uint32_t const*const w = (uint32_t*) &x;

  size_t const e_idx = 23;

  uint32_t const s_mask = 0x80000000;
  uint32_t const e_mask = 0xff;
  uint32_t const f_mask = 0x7fffff;

  uint32_t const s = *w & s_mask;
  uint32_t const s_c = s ^ s_mask;

  uint32_t const e_max = e_mask - 1;
  uint32_t const e_min = f_mask + 1;
  uint32_t const e = (*w >> e_idx) & e_mask;
  uint32_t const e1 = e + 1;

  uint32_t const f_max = f_mask;
  uint32_t const f = *w & f_mask;
  uint32_t const f1 = f + 1;

  uint32_t w_r = *w;
  if (isnormal(x)) {
    if (f1 <= f_max) {
      w_r += 1;
    } else if (e1 <= e_max) {
      w_r = s + (e1 << e_idx);
    } else {
      w_r = s_c + e_min;
    }
  } else {
    if (f1 <= f_max) {
      w_r += 1;
    } else if (s == 0) {
      w_r = s_mask + 1;
    } else {
      w_r = 0;
    }
  }

  float const*const x_r = (float*) &w_r;
  return *x_r;
}

double hsbg_transform_CDouble(double x) {
  if (isnan(x))               return -0.0;
  if (x == 0.0 && signbit(x)) return NAN;
  if (isinf(x))               return -x;

  uint64_t const*const w = (uint64_t*) &x;

  size_t const e_idx = 52;

  uint64_t const s_mask = 0x8000000000000000;
  uint64_t const e_mask = 0x7ff;
  uint64_t const f_mask = 0xfffffffffffff;

  uint64_t const s = *w & s_mask;
  uint64_t const s_c = s ^ s_mask;

  uint64_t const e_max = e_mask - 1;
  uint64_t const e_min = f_mask + 1;
  uint64_t const e = (*w >> e_idx) & e_mask;
  uint64_t const e1 = e + 1;

  uint64_t const f_max = f_mask;
  uint64_t const f = *w & f_mask;
  uint64_t const f1 = f + 1;

  uint64_t w_r = *w;
  if (isnormal(x)) {
    if (f1 <= f_max) {
      w_r += 1;
    } else if (e1 <= e_max) {
      w_r = s + (e1 << e_idx);
    } else {
      w_r = s_c + e_min;
    }
  } else {
    if (f1 <= f_max) {
      w_r += 1;
    } else if (s == 0) {
      w_r = s_mask + 1;
    } else {
      w_r = 0;
    }
  }

  double const*const x_r = (double*) &w_r;
  return *x_r;
}
