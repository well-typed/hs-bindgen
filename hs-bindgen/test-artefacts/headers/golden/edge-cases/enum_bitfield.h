// Test for issue #2403: enum bitfield should get Bitfield instance
enum E { A, B };

struct S {
  enum E e : 2;
};
