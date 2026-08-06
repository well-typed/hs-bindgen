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

// Obviously, this is not a very good hash
__attribute__ ((pure)) int hash (char * s) {
  return *s;
}

__attribute__ ((const)) int square (int x) {
  return x * x;
}
