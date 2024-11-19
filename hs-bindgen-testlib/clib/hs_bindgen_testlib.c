#include <math.h>
#include <stddef.h>
#include <stdint.h>

#include "hs_bindgen_testlib.h"

/*******************************************************************************
  Transform
*******************************************************************************/

char hsbg_transform_CChar(char c) {
  return c + 1;
}

int hsbg_transform_CInt(int n) {
  return n + 1;
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
