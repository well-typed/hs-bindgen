#include <math.h>
#include <signal.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
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
  Preturb
*******************************************************************************/

char hsbg_preturb_CChar(long size, char c) {
  return c + size;
}

signed char hsbg_preturb_CSChar(long size, signed char c) {
  return c + size;
}

unsigned char hsbg_preturb_CUChar(long size, unsigned char c) {
  return c + size;
}

short hsbg_preturb_CShort(long size, short n) {
  return n + size;
}

unsigned short hsbg_preturb_CUShort(long size, unsigned short n) {
  return n + size;
}

int hsbg_preturb_CInt(long size, int n) {
  return n + size;
}

unsigned int hsbg_preturb_CUInt(long size, unsigned int n) {
  return n + size;
}

long hsbg_preturb_CLong(long size, long n) {
  return n + size;
}

unsigned long hsbg_preturb_CULong(long size, unsigned long n) {
  return n + size;
}

ptrdiff_t hsbg_preturb_CPtrdiff(long size, ptrdiff_t n) {
  return n + size;
}

size_t hsbg_preturb_CSize(long size, size_t n) {
  return n + size;
}

wchar_t hsbg_preturb_CWchar(long size, wchar_t c) {
  return c + size;
}

sig_atomic_t hsbg_preturb_CSigAtomic(long size, sig_atomic_t n) {
  return n + size;
}

long long hsbg_preturb_CLLong(long size, long long n) {
  return n + size;
}

unsigned long long hsbg_preturb_CULLong(long size, unsigned long long n) {
  return n + size;
}

bool hsbg_preturb_CBool(long size, bool b) {
  return (abs(size) % 2 == 1) != b;
}

intptr_t hsbg_preturb_CIntPtr(long size, intptr_t n) {
  return n + size;
}

uintptr_t hsbg_preturb_CUIntPtr(long size, uintptr_t n) {
  return n + size;
}

intmax_t hsbg_preturb_CIntMax(long size, intmax_t n) {
  return n + size;
}

uintmax_t hsbg_preturb_CUIntMax(long size, uintmax_t n) {
  return n + size;
}

clock_t hsbg_preturb_CClock(long size, clock_t c) {
  return c + size;
}

time_t hsbg_preturb_CTime(long size, time_t t) {
  return t + size;
}

float hsbg_preturb_CFloat(long size, float x) {
  if (isnan(x)) {
    if (abs(size) % 2 == 0) { return NAN;  } else { return -0.0; }
  }
  if (x == 0.0 && signbit(x)) {
    if (abs(size) % 2 == 0) { return -0.0; } else { return NAN; }
  }
  if (isinf(x)) {
    if (abs(size) % 2 == 0) { return x;    } else { return -x; }
  }

  uint32_t const s_mask = 0x80000000;
  uint32_t const e_mask = 0xff;
  uint32_t const f_mask = 0x7fffff;

  size_t const e_idx = 23;

  long const e_size = e_mask - 1;
  long const f_size = f_mask + 1;

  uint32_t const*const w = (uint32_t*) &x;
  uint32_t const s = *w & s_mask;
  uint32_t const s_c = s ^ s_mask;
  uint32_t const e = (*w >> e_idx) & e_mask;
  uint32_t const f = *w & f_mask;

  ldiv_t const size_qr = ldiv(size, f_size);
  long const size_div = size_qr.quot - (size_qr.rem >= 0 ? 0 : 1);
  long const size_mod = size_qr.rem  + (size_qr.rem >= 0 ? 0 : f_size);

  // positive => quot ~ div, rem ~ mod
  ldiv_t const f_qr = ldiv(size_mod + f, f_size);

  uint32_t w_r = 0;
  if (e == 0) {
    uint32_t const s_r = (size_div + f_qr.quot) % 2 == 0 ? s : s_c;
    w_r = s_r + f_qr.rem;
  } else {
    ldiv_t const e_qr = ldiv(f_qr.quot + size_div + e - 1, e_size);
    long const e_div = e_qr.quot - (e_qr.rem >= 0 ? 0 : 1);
    uint32_t const e_mod = e_qr.rem + (e_qr.rem >= 0 ? 0 : e_size) + 1;
    uint32_t const s_r = e_div % 2 == 0 ? s : s_c;
    w_r = s_r + (e_mod << e_idx) + f_qr.rem;
  }

  float const*const x_r = (float*) &w_r;
  return *x_r;
}

double hsbg_preturb_CDouble(long size, double x) {
  if (isnan(x)) {
    if (abs(size) % 2 == 0) { return NAN;  } else { return -0.0; }
  }
  if (x == 0.0 && signbit(x)) {
    if (abs(size) % 2 == 0) { return -0.0; } else { return NAN; }
  }
  if (isinf(x)) {
    if (abs(size) % 2 == 0) { return x;    } else { return -x; }
  }

  uint64_t const s_mask = 0x8000000000000000;
  uint64_t const e_mask = 0x7ff;
  uint64_t const f_mask = 0xfffffffffffff;

  size_t const e_idx = 52;

  long const e_size = e_mask - 1;
  long const f_size = f_mask + 1;

  uint64_t const*const w = (uint64_t*) &x;
  uint64_t const s = *w & s_mask;
  uint64_t const s_c = s ^ s_mask;
  uint64_t const e = (*w >> e_idx) & e_mask;
  uint64_t const f = *w & f_mask;

  ldiv_t const size_qr = ldiv(size, f_size);
  long const size_div = size_qr.quot - (size_qr.rem >= 0 ? 0 : 1);
  long const size_mod = size_qr.rem  + (size_qr.rem >= 0 ? 0 : f_size);

  // positive => quot ~ div, rem ~ mod
  ldiv_t const f_qr = ldiv(size_mod + f, f_size);

  uint64_t w_r = 0;
  if (e == 0) {
    uint64_t const s_r = (size_div + f_qr.quot) % 2 == 0 ? s : s_c;
    w_r = s_r + f_qr.rem;
  } else {
    ldiv_t const e_qr = ldiv(f_qr.quot + size_div + e - 1, e_size);
    long const e_div = e_qr.quot - (e_qr.rem >= 0 ? 0 : 1);
    uint64_t const e_mod = e_qr.rem + (e_qr.rem >= 0 ? 0 : e_size) + 1;
    uint64_t const s_r = e_div % 2 == 0 ? s : s_c;
    w_r = s_r + (e_mod << e_idx) + f_qr.rem;
  }

  double const*const x_r = (double*) &w_r;
  return *x_r;
}