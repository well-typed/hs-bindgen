/* Qualified void result types */

typedef int T;

const void f(void);

typedef const void F(void);

void n(const void (*x)(T));
