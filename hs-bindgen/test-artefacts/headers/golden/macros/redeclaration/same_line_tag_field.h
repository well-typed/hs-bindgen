// Regression test: a macro expansion in the struct *tag* must not be
// mis-attributed to a *field* that happens to be on the same source line.
//
// `A` is defined twice (identically) and expands to the struct tag `S`; `B`
// expands to the field type `int`. Both expansions sit on line 4. Reparse
// info is attached to the field, not the struct, so only `B` should be
// recorded for the field. Previously `SourceRangeMap` keyed lookups by line
// only, so the field also picked up the (redefined, hence "ambiguous") `A`,
// producing a spurious "expansion not unique" warning and an "unknown type of
// expanded macro A" error. With column-precise attribution the field sees
// only `B`.
#define A S
#define A S
#define B int
struct A {B x;};
