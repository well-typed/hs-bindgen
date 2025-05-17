// Example that shows off the various possibilities
// Perhaps this would be a good example of the Dep-Use graph for the paper
typedef struct {
  struct {
    struct {
      int a;
      int b;
    } *innermost;
  } inner1;
  struct {
    int c;
    int d;
  } inner2;
  int z;
} *toplevel;

