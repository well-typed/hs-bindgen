/**
 * This is a test case in the c2hsc test suite
 *
 * https://github.com/jwiegley/c2hsc/blob/master/test/main.hs
 */

typedef unsigned int uint;
typedef unsigned long size_t;

void    foo1(void);
void    foo2(int);
void    foo3(int, int);
int     foo4(void);
char    foo5(int);
char *  foo6(int, int);
char *  foo7(char *);
char *  foo8(char * b);
char *  foo9(char * (*b)(void));
char *  foo10(char * (*b)(int));
void *  foo11(void * (*b)(void));
void *  foo12(void * (*b)(int));
char *  foo13(char []);
char *  foo14(char b[]);
char *  foo15(char b[5]);
char *  foo16(int);
int     foo17(char ***);
int     foo18(unsigned);
int     foo19(unsigned int);
int     foo20(uint);
int     foo21(int (*)(int));
int     foo22(int *(*)(int));
int     foo23(int **(*)(int));
int     foo24(int ***(*)(int));
int *   foo25(int);
int **  foo26(int);
int *** foo27(int);
int *** foo28(size_t);

struct bar1_t {
  void *  a;
  int     b;
  char    c;
  char *  d;
  char *  (*e)(void);
  void    (*f)(void *);
  int *   (*g)(void *);
  int **  (*h)(void *);
  int *** (*i)(void *);
  char    j[2];

  struct bar1_t * k;
};

typedef struct bar2_t {
  int a;
} bar2_t;

typedef struct {
  int a;
} bar3_t;

enum {
  BAZ1 = 1
};

typedef enum {
  BAZ2 = 1
} baz2_t;

enum baz3_t {
  BAZ3 = 1
};

typedef enum baz4_t {
  BAZ4 = 1
} baz4_t;

extern int global;

inline int inline_foo(int a, int * b, const int c, const int * d,
                      const int ** e, const int * const * f, size_t g) {
  return 10;
}
