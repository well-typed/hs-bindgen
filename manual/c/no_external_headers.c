#include "no_external_headers.h"

void mk_triple(int a, int b, int c, triple* triple) {
    (*triple).a = a;
    (*triple).b = b;
    (*triple).c = c;
}

int index_triple(triple* triple, index ix) {
    switch(ix) {
        case A: return triple->a;
        case B: return triple->b;
        case C: return triple->c;
    }
}

sum sum_triple(triple* triple) {
    return triple->a + triple->b + triple->c;
}

average average_triple(triple* triple) {
    return (double)(sum_triple(triple)) / 3.0;
}

YEAR getYear(date* d) {
    return d->year;
}
