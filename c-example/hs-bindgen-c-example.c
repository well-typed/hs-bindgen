#include <stdio.h>

#include "hs-bindgen-c-example.h"

void hs_bindgen_c_example_helloworld()
{
    printf("hs_bindgen_c_example_helloworld\n");
}

void hs_bindgen_c_example_showInt(int i)
{
    printf("hs_bindgen_c_example_showInt(%d)\n", i);
}

void hs_bindgen_c_example_showStruct(ExampleStruct* s)
{
    printf("hs_bindgen_c_example_showStruct(%d,%d)\n", s->a, s->b);
}

void hs_bindgen_c_example_callFunPtr(void (*funPtr)(int)) {
    printf("hs_bindgen_c_example_showInt(callFunPtr)\n");
    printf("  "); (*funPtr)(1234);
    printf("  "); (*funPtr)(5678);
}

