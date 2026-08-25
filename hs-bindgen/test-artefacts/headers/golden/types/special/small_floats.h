/*
 * Small floating-point types are not supported; we warn about them.
 *
 * __ibm128 is only available on PowerPC targets, and __bf16 is exposed
 * differently depending on the target, so neither is tested here.
 */

typedef _Float16 float16_t;
typedef __fp16 fp16_t;

struct small_floats {
  _Float16 f16;
  __fp16 fp16;
};

_Float16 fun(int x);
