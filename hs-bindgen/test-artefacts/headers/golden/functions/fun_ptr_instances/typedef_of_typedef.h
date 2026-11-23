/* A typedef of a typedef of a function type */

typedef int T;

typedef void f(T);

// No instances: f already has them
typedef f g;
