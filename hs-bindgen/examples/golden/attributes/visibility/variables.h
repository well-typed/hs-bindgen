/*
 * Visibility attributes on variable declarations
 *
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Attributes.html#index-visibility
 *
 * The example declarations are themselves not terribly interesting.
 * Nonetheless, they are intended to be extensive enough to cover most (if not
 * all) interactions between visibility attributes and other aspects that modify
 * variable declarations, such as: linkage, storage class, variable
 * initialisers, tenative definitions, etc.
 */

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
