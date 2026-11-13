/* A typedef of a typedef of a function type */

typedef int T;

typedef void f(T);

// No instances: see the instances of f
typedef f g;
