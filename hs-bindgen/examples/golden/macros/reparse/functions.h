#define ID(X) (X)

// We fail to reparse `foo`:
//
// The symbol `x' does not fit here.
void foo(int ID(x));

// We fail to reparse `bar`:
//
// Encountered unexpected node in the language-c AST:  CStatic NodeOmitted.
static inline int bar(int x) {
  int y = 10;
  return ID(y);
}
