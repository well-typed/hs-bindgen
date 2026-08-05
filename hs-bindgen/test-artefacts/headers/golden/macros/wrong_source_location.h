/* Regression test for issue #1860.
 *
 * `NestedUnnamed` (from the ticket) and `TwoUntaggedStructs` each expand to
 * multiple unnamed declarations from a single macro invocation. libclang
 * reports the same expansion location for all of them, so without spelling
 * locations they would collide on the same UnnamedId.
 *
 * `NestedUnnamed` only collides on clang < 16 (newer clangs implicitly name
 * the typedef'd outer struct). `TwoUntaggedStructs` collides on every clang
 * version, since both inner structs are genuinely untagged.
 */
#define NestedUnnamed(t1, n1, t2) \
  typedef t1 {                    \
    t2 {                          \
      int fieldX;                 \
    } fieldY;                     \
  } n1;

NestedUnnamed(struct, UU1, struct)

NestedUnnamed(struct, UU2, struct)

#define TwoUntaggedStructs(name)         \
  struct name {                     \
    struct { int a; } fieldA;       \
    struct { int b; } fieldB;       \
  };

TwoUntaggedStructs(VV1)

TwoUntaggedStructs(VV2)
