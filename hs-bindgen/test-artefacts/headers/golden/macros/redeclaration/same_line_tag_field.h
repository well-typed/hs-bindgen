// Regression test: a macro expansion in the struct *tag* must not be
// mis-attributed to a *field* that happens to be on the same source line.
//
// `A` is defined twice (differently, with an `#undef` in between, which
// hs-bindgen does not see) and expands to the struct tag `S`; `B` expands to
// the field type `int`. Both expansions sit on the same line. Reparse info is
// attached to the field, not the struct, so only `B` should be recorded for the
// field. Previously `SourceRangeMap` keyed lookups by line only, so the field
// also picked up the ambiguous `A`, producing a spurious "expansion not unique"
// warning and an "unknown type of expanded macro A" error. With column-precise
// attribution the field sees only `B`.
#define A R
#undef A
#define A S
#define B int
struct A {B x;};
