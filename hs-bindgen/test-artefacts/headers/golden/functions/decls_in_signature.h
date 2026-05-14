// regular use of structs in function signatures
struct opaque;
struct outside {
  int x;
  int y;
};
void normal(struct opaque* ptr_to_opaque, struct outside* ptr_to_defined, struct outside by_value);

/**
 * Error cases
 *
 * See 'UnexpectedAnonInSignature' for discussion (of both these error cases
 * and the edge cases below).
 */

// named struct declared inline
void f1(struct named_struct { int x; int y; } arg);

// named union
void f2(union named_union { int x; char y; } arg);

/**
 * Edge cases: these result in warnings from clang, but in principle we can
 * generate bindings for these.
 */

// anonymous struct declared inline
void f3(struct { int x; int y; } arg);

// _multiple_ anonymous structs declared inline
void f4(struct { int x; int y; } p1, struct { int x; int y; } p2);

// anonymous union
void f5(union { int x; char y; } arg);
