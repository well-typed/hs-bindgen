/**
 * Visibility attributes on functions
 *
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-visibility-function-attribute
 */


void f0 ();

void __attribute__ ((visibility ("default"))) f1 ();
void __attribute__ ((visibility ("hidden"))) f2 ();
void __attribute__ ((visibility ("internal"))) f3 ();
void __attribute__ ((visibility ("protected"))) f4 ();

int i0;

int __attribute__ ((visibility ("default"))) i1;
int __attribute__ ((visibility ("hidden"))) i2;
int __attribute__ ((visibility ("internal"))) i3;
int __attribute__ ((visibility ("protected"))) i4;

