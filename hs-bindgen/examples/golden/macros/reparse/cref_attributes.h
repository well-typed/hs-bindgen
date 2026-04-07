// simple case
#define BOOL int
void foo(BOOL);

// attributes using C reference syntax
[[gnu::visibility("default")]] void bar(BOOL);
// void [[gnu::visibility("default")]] dash(BOOL); // not allowed!

// attributes using C reference syntax in a macro definition
#define EXPORT [[gnu::visibility("default")]]
EXPORT void quux (BOOL);
// void EXPORT heq(BOOL); // not allowed!