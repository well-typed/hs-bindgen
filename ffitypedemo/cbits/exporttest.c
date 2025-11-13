#include "exporttest.h"

#include <stdio.h>

void foo(int i) {
    printf("foo: %d\n", i);
}

void bar(struct SomeStruct *s) {
    printf("bar: %d %d\n", s->i, s->j);
}
