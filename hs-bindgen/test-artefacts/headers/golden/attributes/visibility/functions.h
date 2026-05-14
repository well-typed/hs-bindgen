/*
 * Visibility attributes on function declarations
 *
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Attributes.html#index-visibility
 *
 * The example declarations are themselves not terribly interesting.
 * Nonetheless, they are intended to be extensive enough to cover most (if not
 * all) interactions between visibility attributes and other aspects that modify
 * function declarations, such as: linkage, storage class, function definitions,
 * etc.
 */

// implicit extern
void                                                   f0 (void);
void        __attribute__ ((visibility ("default")))   f1 (void);
void        __attribute__ ((visibility ("hidden")))    f2 (void);
void        __attribute__ ((visibility ("internal")))  f3 (void);
void        __attribute__ ((visibility ("protected"))) f4 (void);

// implicit extern, definition included
void                                                   f5 (void) {return;};
void        __attribute__ ((visibility ("default")))   f6 (void) {return;};
void        __attribute__ ((visibility ("hidden")))    f7 (void) {return;};
void        __attribute__ ((visibility ("internal")))  f8 (void) {return;};
void        __attribute__ ((visibility ("protected"))) f9 (void) {return;};

// extern
extern void                                            f10 (void);
extern void __attribute__ ((visibility ("default")))   f11 (void);
extern void __attribute__ ((visibility ("hidden")))    f12 (void);
extern void __attribute__ ((visibility ("internal")))  f13 (void);
extern void __attribute__ ((visibility ("protected"))) f14 (void);

// extern, definition included
extern void                                            f15 (void) {return;};
extern void __attribute__ ((visibility ("default")))   f16 (void) {return;};
extern void __attribute__ ((visibility ("hidden")))    f17 (void) {return;};
extern void __attribute__ ((visibility ("internal")))  f18 (void) {return;};
extern void __attribute__ ((visibility ("protected"))) f19 (void) {return;};

// static
static void                                            f20 (void);
static void __attribute__ ((visibility ("default")))   f21 (void);
static void __attribute__ ((visibility ("hidden")))    f22 (void);
static void __attribute__ ((visibility ("internal")))  f23 (void);
static void __attribute__ ((visibility ("protected"))) f24 (void);

// static, definition included
static void                                            f25 (void) {return;};
static void __attribute__ ((visibility ("default")))   f26 (void) {return;};
static void __attribute__ ((visibility ("hidden")))    f27 (void) {return;};
static void __attribute__ ((visibility ("internal")))  f28 (void) {return;};
static void __attribute__ ((visibility ("protected"))) f29 (void) {return;};

