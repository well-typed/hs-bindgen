/*
 * Visibility attributes on functions and global variables
 *
 * https://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-visibility-function-attribute
 */


void f0 ();

void __attribute__ ((visibility ("default"))) f1 (void);
void __attribute__ ((visibility ("hidden"))) f2 (void);
void __attribute__ ((visibility ("internal"))) f3 (void);
void __attribute__ ((visibility ("protected"))) f4 (void);

void f5 (void) {return;};

void __attribute__ ((visibility ("default"))) f6 (void) {return;};
void __attribute__ ((visibility ("hidden"))) f7 (void) {return;};
void __attribute__ ((visibility ("internal"))) f8 (void) {return;};
void __attribute__ ((visibility ("protected"))) f9 (void) {return;};

int i0;

int __attribute__ ((visibility ("default"))) i1;
int __attribute__ ((visibility ("hidden"))) i2;
int __attribute__ ((visibility ("internal"))) i3;
int __attribute__ ((visibility ("protected"))) i4;

extern int i5;

extern int __attribute__ ((visibility ("default"))) i6;
extern int __attribute__ ((visibility ("hidden"))) i7;
extern int __attribute__ ((visibility ("internal"))) i8;
extern int __attribute__ ((visibility ("protected"))) i9;

static int i10;

static int __attribute__ ((visibility ("default"))) i11;
static int __attribute__ ((visibility ("hidden"))) i12;
static int __attribute__ ((visibility ("internal"))) i13;
static int __attribute__ ((visibility ("protected"))) i14;
