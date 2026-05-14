#define ID(X) (X)
#define MY_INT int

// We fail to reparse `foo`:
//
// The symbol `x' does not fit here.
void foo(int ID(x));

static inline int bar(int x) {
  int y = 10;
  return ID(y);
}

extern int baz(int x) {
  int y = 10;
  return ID(y);
}

typedef MY_INT my_int_t;
