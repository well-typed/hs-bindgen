#include "manual_examples.h"

#include <stdio.h>

/**
  * Simple struct
  */

void mk_triple(int a, int b, int c, triple* triple) {
    (*triple).a = a;
    (*triple).b = b;
    (*triple).c = c;
}

/**
 * Simple enum
 */

int index_triple(triple* triple, index ix) {
    switch(ix) {
        case A: return triple->a;
        case B: return triple->b;
        case C: return triple->c;
    }
    return 0;
}

/**
 * Typedefs
 */

sum sum_triple(triple* triple) {
    return triple->a + triple->b + triple->c;
}

average average_triple(triple* triple) {
    return (double)(sum_triple(triple)) / 3.0;
}

/**
 * Macros
 */

YEAR getYear(date* d) {
    return d->year;
}

/**
 * Unions
 */

void print_occupation(int tag, occupation* o) {
    switch(tag) {
        case 0:
            printf("student in year %d\n", o->student.year);
            break;
        case 1:
            printf("employee with salary %d\n", o->employee.salary);
            break;
        default:
            printf("unknown tag\n");
            break;
    };
}


/**
 * Awkward names
 */

#if defined(__APPLE__)
  // Required since Apple's GCC Assembler does not allow non-ASCII characters
  void ByeBye() {
      printf("This is the ByeBye function (macOS version).\n");
  }
  void Gamma() {
      printf("This is the Gamma function (macOS version).\n");
  }
#else
  void 拜拜(void) {
      printf("C function '拜拜' (byebye)\n");
  }

  void ϒ(void) {
      printf("C function 'ϒ' (U+03D2 Greek Upsilon with Hook Symbol)\n");
  }
#endif

void import(void) {
    printf("C function 'import'\n");
}

