typedef int T1;
typedef char T2;

#define M1 int
#define M2 char
#define M3 int[3]
#define M4 int*

struct ExampleStruct {
  T1 t1;
  T2 t2;
  M1 m1;
  M2 m2;
};

#define uint64_t int

struct foo {
  uint64_t* a;
};
