/* Sweep-2 collision: struct foo generates the auxiliary type Foo_Aux (for its
   flexible array member), which clashes with the top-level declaration foo_Aux
   that also mangles to Foo_Aux. */
struct foo { int len; char data[]; };
typedef int foo_Aux;

/* Sweep-2 collision: function-pointer typedef bar generates the auxiliary type
   Bar_Aux, which clashes with the top-level declaration bar_Aux. */
typedef void (*bar)(int x);
typedef int bar_Aux;
