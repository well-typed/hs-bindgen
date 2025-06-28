// regular use of structs in function signatures
struct opaque;
struct outside {
  int x;
  int y;
};
void normal(struct opaque* ptr_to_opaque, struct outside* ptr_to_defined, struct outside by_value);

// named struct declared inline
void f1(struct named { int x; int y; } arg);

// anonymous struct declared inline
void f2(struct { int x; int y; } arg);

// _multiple_ anonymous structs declared inline
void f3(struct { int x; int y; } p1, struct { int x; int y; } p2);

// named union
void f4(union named_union { int x; char y; } arg);

// anonymous union
void f5(union { int x; char y; } arg);
