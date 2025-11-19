/**
 * Examples from the initial specification
 */

// Preliminaries to make the examples typecheck
//
// Some of these definitions are taken from
// https://github.com/analogdevicesinc/MathWorks_tools/blob/master/targeting_models/ADSB/linux_app/rtwtypes.h

typedef short int16_T;
typedef int int32_T;
typedef long long int64_T;

typedef struct {
  int16_T re;
  int16_T im;
} cint16_T;

struct B {};

// Examples themselves

struct A {
  double x;
  char* label;
  char samples[128];
  struct B b;
  struct C* c;
};

void resample(
  int32_T *res_m_num_valid_samples,
  cint16_T res_m_iq_int[30720000],
  int64_T res_m_old_rate,
  int64_T res_m_new_rate,
  cint16_T res_m_iq_resampled_int[30720000]
);