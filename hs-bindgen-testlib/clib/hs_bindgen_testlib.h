#ifndef HS_BINDGEN_TESTLIB_H
#define HS_BINDGEN_TESTLIB_H

#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <sys/types.h>
#include <time.h>

/*******************************************************************************
  HsBindgen.TestLib.Transform
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

long hsbg_transform_CIntPtr(long);

unsigned long hsbg_transform_CUIntPtr(unsigned long);

long hsbg_transform_CIntMax(long);

unsigned long hsbg_transform_CUIntMax(unsigned long);

clock_t hsbg_transform_CClock(clock_t);

time_t hsbg_transform_CTime(time_t);

/* TODO remove or fix
useconds_t hsbg_transform_CUSeconds(useconds_t);
*/

suseconds_t hsbg_transform_CSUSeconds(suseconds_t);

float hsbg_transform_CFloat(float);

double hsbg_transform_CDouble(double);

#endif // HS_BINDGEN_TESTLIB_H
