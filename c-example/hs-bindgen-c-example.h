#ifndef EXAMPLE_H
#define EXAMPLE_H

typedef struct ExampleStruct {
    int a;
    int b;
} ExampleStruct;

void hs_bindgen_c_example_helloworld();
void hs_bindgen_c_example_showInt(int);
void hs_bindgen_c_example_showStruct(ExampleStruct*);

#endif