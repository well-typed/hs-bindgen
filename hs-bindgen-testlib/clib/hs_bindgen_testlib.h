#ifndef HS_BINDGEN_TESTLIB_H
#define HS_BINDGEN_TESTLIB_H

#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>

/*******************************************************************************
  Storable: sizeof
*******************************************************************************/

size_t hsbg_sizeof_CChar(void);

size_t hsbg_sizeof_CSChar(void);

size_t hsbg_sizeof_CUChar(void);

size_t hsbg_sizeof_CShort(void);

size_t hsbg_sizeof_CUShort(void);

size_t hsbg_sizeof_CInt(void);

size_t hsbg_sizeof_CUInt(void);

size_t hsbg_sizeof_CLong(void);

size_t hsbg_sizeof_CULong(void);

size_t hsbg_sizeof_CPtrdiff(void);

size_t hsbg_sizeof_CSize(void);

size_t hsbg_sizeof_CWchar(void);

size_t hsbg_sizeof_CSigAtomic(void);

size_t hsbg_sizeof_CLLong(void);

size_t hsbg_sizeof_CULLong(void);

size_t hsbg_sizeof_CBool(void);

size_t hsbg_sizeof_CIntPtr(void);

size_t hsbg_sizeof_CUIntPtr(void);

size_t hsbg_sizeof_CIntMax(void);

size_t hsbg_sizeof_CUIntMax(void);

size_t hsbg_sizeof_CClock(void);

size_t hsbg_sizeof_CTime(void);

size_t hsbg_sizeof_CFloat(void);

size_t hsbg_sizeof_CDouble(void);

/*******************************************************************************
  Storable: alignof
*******************************************************************************/

size_t hsbg_alignof_CChar(void);

size_t hsbg_alignof_CSChar(void);

size_t hsbg_alignof_CUChar(void);

size_t hsbg_alignof_CShort(void);

size_t hsbg_alignof_CUShort(void);

size_t hsbg_alignof_CInt(void);

size_t hsbg_alignof_CUInt(void);

size_t hsbg_alignof_CLong(void);

size_t hsbg_alignof_CULong(void);

size_t hsbg_alignof_CPtrdiff(void);

size_t hsbg_alignof_CSize(void);

size_t hsbg_alignof_CWchar(void);

size_t hsbg_alignof_CSigAtomic(void);

size_t hsbg_alignof_CLLong(void);

size_t hsbg_alignof_CULLong(void);

size_t hsbg_alignof_CBool(void);

size_t hsbg_alignof_CIntPtr(void);

size_t hsbg_alignof_CUIntPtr(void);

size_t hsbg_alignof_CIntMax(void);

size_t hsbg_alignof_CUIntMax(void);

size_t hsbg_alignof_CClock(void);

size_t hsbg_alignof_CTime(void);

size_t hsbg_alignof_CFloat(void);

size_t hsbg_alignof_CDouble(void);

/*******************************************************************************
  Preturb
*******************************************************************************/

char hsbg_preturb_CChar(long, char);

signed char hsbg_preturb_CSChar(long, signed char);

unsigned char hsbg_preturb_CUChar(long, unsigned char);

short hsbg_preturb_CShort(long, short);

unsigned short hsbg_preturb_CUShort(long, unsigned short);

int hsbg_preturb_CInt(long, int);

unsigned int hsbg_preturb_CUInt(long, unsigned int);

long hsbg_preturb_CLong(long, long);

unsigned long hsbg_preturb_CULong(long, unsigned long);

ptrdiff_t hsbg_preturb_CPtrdiff(long, ptrdiff_t);

size_t hsbg_preturb_CSize(long, size_t);

wchar_t hsbg_preturb_CWchar(long, wchar_t);

sig_atomic_t hsbg_preturb_CSigAtomic(long, sig_atomic_t);

long long hsbg_preturb_CLLong(long, long long);

unsigned long long hsbg_preturb_CULLong(long, unsigned long long);

bool hsbg_preturb_CBool(long, bool);

intptr_t hsbg_preturb_CIntPtr(long, intptr_t);

uintptr_t hsbg_preturb_CUIntPtr(long, uintptr_t);

intmax_t hsbg_preturb_CIntMax(long, intmax_t);

uintmax_t hsbg_preturb_CUIntMax(long, uintmax_t);

clock_t hsbg_preturb_CClock(long, clock_t);

time_t hsbg_preturb_CTime(long, time_t);

float hsbg_preturb_CFloat(long, float);

double hsbg_preturb_CDouble(long, double);

#endif // HS_BINDGEN_TESTLIB_H
