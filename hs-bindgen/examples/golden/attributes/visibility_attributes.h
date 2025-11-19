/*
 * Visibility attributes on declarations for functions and global variables
 *
 * https://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-visibility-function-attribute
 *
 * The example declarations for functions and global variables below are
 * themselves not terribly interesting. Nonetheless, they are intended to be
 * extensive enough to cover most (if not all) interactions between visibility
 * attributes and other aspects that modify declarations, linkage, storage
 * class, variable initialisers, function definitions, etc.
 */

// Function declarations (i.e., function prototypes)

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

// Global variable declarations

// non-extern, non-static, tentative definition
int                                            i0;
int __attribute__ ((visibility ("default")))   i1;
int __attribute__ ((visibility ("hidden")))    i2;
int __attribute__ ((visibility ("internal")))  i3;
int __attribute__ ((visibility ("protected"))) i4;

// non-extern, non-static, definition included
int                                            i5 = 5;
int __attribute__ ((visibility ("default")))   i6 = 6;
int __attribute__ ((visibility ("hidden")))    i7 = 7;
int __attribute__ ((visibility ("internal")))  i8 = 8;
int __attribute__ ((visibility ("protected"))) i9 = 9;

// extern
extern int                                            i10;
extern int __attribute__ ((visibility ("default")))   i11;
extern int __attribute__ ((visibility ("hidden")))    i12;
extern int __attribute__ ((visibility ("internal")))  i13;
extern int __attribute__ ((visibility ("protected"))) i14;

// extern, definition included
extern int                                            i15 = 15;
extern int __attribute__ ((visibility ("default")))   i16 = 16;
extern int __attribute__ ((visibility ("hidden")))    i17 = 17;
extern int __attribute__ ((visibility ("internal")))  i18 = 18;
extern int __attribute__ ((visibility ("protected"))) i19 = 19;

// static, tentative definition
static int                                            i20;
static int __attribute__ ((visibility ("default")))   i21;
static int __attribute__ ((visibility ("hidden")))    i22;
static int __attribute__ ((visibility ("internal")))  i23;
static int __attribute__ ((visibility ("protected"))) i24;

// static, definition included
static int                                            i25 = 25;
static int __attribute__ ((visibility ("default")))   i26 = 26;
static int __attribute__ ((visibility ("hidden")))    i27 = 27;
static int __attribute__ ((visibility ("internal")))  i28 = 28;
static int __attribute__ ((visibility ("protected"))) i29 = 29;

// deprecated
void deprecated_function(void)
  __attribute__((deprecated("This function is deprecated. There is no replacement for this function")));
