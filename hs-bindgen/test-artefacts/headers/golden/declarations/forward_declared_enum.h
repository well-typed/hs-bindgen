/* Forward-declared enum without a definition (GNU extension).
   Functions that use it by value are unusable: the integer type is unknown.
   https://github.com/well-typed/hs-bindgen/issues/2353 */

enum E;
void takes_e(enum E x);
enum E returns_e(void);

/* A pointer to a forward-declared enum is also incomplete. */
void takes_e_ptr(enum E *x);
