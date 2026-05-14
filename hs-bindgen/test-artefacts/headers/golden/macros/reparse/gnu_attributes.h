// simple case
#define BOOL int
void foo(BOOL);

// attributes using GNU syntax
__attribute__ ((visibility("default"))) void bar(BOOL);
void __attribute__ ((visibility("default"))) dash(BOOL);

// attributes using GNU syntax in a macro definition
#define EXPORT __attribute__ ((visibility("default")))
EXPORT void quux(BOOL);
void EXPORT heq(BOOL);
