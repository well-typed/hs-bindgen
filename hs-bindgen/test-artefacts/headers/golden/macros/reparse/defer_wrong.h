/*
In case a typedef references another type, we defer reparsing to the referenced
type. This was done to resolve issue #707. However, it now turns out that this
introduced a new bug. A typedef may reference a struct indirectly via a macro
type, in which case we need to reparse the typedef to insert a referene to the
macro type.

<https://github.com/well-typed/hs-bindgen/issues/707>
*/

struct S { int x; }; // CORRECT: data S = ...

#define T struct S // CORRECT: newtype T = T S

typedef T foo; // WRONG: newtype Foo = Foo S
typedef T * bar; // CORRECT: newtype Bar = Bar (Ptr T)
