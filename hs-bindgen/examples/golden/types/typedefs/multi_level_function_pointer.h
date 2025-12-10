/*
 * Multi-level function pointer typedefs
 *
 */

/* Single level (regression test - should still work) */
typedef void (*f1)(int x, int y);

/* Double pointer */
typedef void (**f2)(int x, int y);

/* Triple pointer */
typedef void (***f3)(int x, int y);

/* Different return type */
typedef int (**f4)(void);

/* No arguments */
typedef void (**f5)(void);

/* With typedef'd types */
typedef int MyInt;
typedef void (**f6)(MyInt x);
