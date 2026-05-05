/* Regression test for issue #1860.
 *
 * `NestedUnnamed` (from the ticket) and `TwoAnonFields` each expand to
 * multiple anonymous declarations from a single macro invocation. libclang
 * reports the same expansion location for all of them, so without spelling
 * locations they would collide on the same AnonId.
 *
 * `NestedUnnamed` only collides on clang < 16 (newer clangs implicitly name
 * the typedef'd outer struct). `TwoAnonFields` collides on every clang
 * version, since both inner structs are genuinely anonymous.
 */
#define NestedUnnamed(t1, n1, t2) \
  typedef t1 {                    \
    t2 {                          \
      int fieldX;                 \
    } fieldY;                     \
  } n1;

NestedUnnamed(struct, UU1, struct)

NestedUnnamed(struct, UU2, struct)

#define TwoAnonFields(name)         \
  struct name {                     \
    struct { int a; } fieldA;       \
    struct { int b; } fieldB;       \
  };

TwoAnonFields(VV1)

TwoAnonFields(VV2)
