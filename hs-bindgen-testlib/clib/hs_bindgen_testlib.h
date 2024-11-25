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
  Transform
*******************************************************************************/

char hsbg_transform_CChar(char);

signed char hsbg_transform_CSChar(signed char);

unsigned char hsbg_transform_CUChar(unsigned char);

short hsbg_transform_CShort(short);

unsigned short hsbg_transform_CUShort(unsigned short);

int hsbg_transform_CInt(int);

unsigned int hsbg_transform_CUInt(unsigned int);

long hsbg_transform_CLong(long);

unsigned long hsbg_transform_CULong(unsigned long);

ptrdiff_t hsbg_transform_CPtrdiff(ptrdiff_t);

size_t hsbg_transform_CSize(size_t);

wchar_t hsbg_transform_CWchar(wchar_t);

sig_atomic_t hsbg_transform_CSigAtomic(sig_atomic_t);

long long hsbg_transform_CLLong(long long);

unsigned long long hsbg_transform_CULLong(unsigned long long);

bool hsbg_transform_CBool(bool);

intptr_t hsbg_transform_CIntPtr(intptr_t);

uintptr_t hsbg_transform_CUIntPtr(uintptr_t);

intmax_t hsbg_transform_CIntMax(intmax_t);

uintmax_t hsbg_transform_CUIntMax(uintmax_t);

clock_t hsbg_transform_CClock(clock_t);

time_t hsbg_transform_CTime(time_t);

float hsbg_transform_CFloat(float);

double hsbg_transform_CDouble(double);

#endif // HS_BINDGEN_TESTLIB_H
