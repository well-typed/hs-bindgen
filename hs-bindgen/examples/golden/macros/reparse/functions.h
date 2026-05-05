#define ID(X) (X)

// We fail to reparse `foo`:
//
// The symbol `x' does not fit here.
void foo(int ID(x));

static inline int bar(int x) {
  int y = 10;
  return ID(y);
}
