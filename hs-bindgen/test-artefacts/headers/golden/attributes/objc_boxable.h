/*
 * The objc_boxable attribute
 *
 * https://clang.llvm.org/docs/ObjectiveCLiterals.html#boxed-c-structures
 *
 * A struct or union with this attribute can be used with the Objective-C boxed
 * expression syntax. The attribute has no meaning in C and does not change the
 * generated bindings.
 */

// struct definition
struct __attribute__ ((objc_boxable)) S0 { double x; double y; };

// redeclaration inside a typedef
struct S1 { double x; double y; };
typedef struct __attribute__ ((objc_boxable)) S1 S1;

// union definition
union __attribute__ ((objc_boxable)) U0 { int i; float f; };
