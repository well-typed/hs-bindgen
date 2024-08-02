#ifndef EXAMPLE_H
#define EXAMPLE_H

typedef struct ExampleStruct {
    int a;
    int b;
} ExampleStruct;

typedef void (*FunPtr_Void_Int)(int);

void hs_bindgen_c_example_helloworld();
void hs_bindgen_c_example_showInt(int);
void hs_bindgen_c_example_showStruct(ExampleStruct*);
void hs_bindgen_c_example_callFunPtr(FunPtr_Void_Int);
FunPtr_Void_Int hs_bindgen_c_example_returnFunPtr();

#endif