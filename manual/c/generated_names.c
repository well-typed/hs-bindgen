#include "generated_names.h"
#include <stdio.h>

/**
 * Awkward names
 */

#if defined(SUPPORTS_UNICODE)
  void 拜拜(void) {
      printf("C function '拜拜' (byebye)\n");
  }

  void ϒ(void) {
      printf("C function 'ϒ' (U+03D2 Greek Upsilon with Hook Symbol)\n");
  }
// Required since Apple's GCC Assembler and LLVM IR does not allow Unicode characters
#else
  void ByeBye() {
      printf("This is the ByeBye function (Unicode-free version).\n");
  }
  void Gamma() {
      printf("This is the Gamma function (Unicode-free version).\n");
  }
#endif

void import(void) {
    printf("C function 'import'\n");
}
