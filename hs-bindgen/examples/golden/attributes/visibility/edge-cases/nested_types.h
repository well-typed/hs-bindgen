
struct __attribute__ ((visibility ("default"))) S {
  union __attribute__ ((visibility ("hidden"))) U {
    int x;
  } y;
};
