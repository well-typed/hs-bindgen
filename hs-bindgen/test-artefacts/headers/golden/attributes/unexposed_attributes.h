/*
 * Attributes without a dedicated libclang cursor kind
 *
 * https://clang.llvm.org/docs/AttributeReference.html
 *
 * libclang names only a few attributes. All others, including the ones below,
 * are reported as an anonymous CXCursor_UnexposedAttr child of the declaration
 * they are attached to. None of them changes the generated bindings.
 */

// global variables
extern int i0 __attribute__ ((swift_private));
extern int i1 __attribute__ ((weak));
extern int i2 __attribute__ ((unused));

// enum
enum __attribute__ ((enum_extensibility (open))) E0 { E0_A, E0_B };

/*
 * Availability attributes
 *
 * GCC rejects the arguments of the availability attribute, so the attribute is
 * only present when the header is read by Clang. hs-bindgen reads the header
 * with Clang, whereas the generated bindings may be compiled with GCC.
 */

#ifdef __clang__

// attribute after the declarator
extern int i3 __attribute__ ((availability (macos, introduced = 10.5)));

// attribute before the declaration specifiers
__attribute__ ((availability (macos, introduced = 10.5))) extern int i4;

// more than one attribute
extern const int i5
  __attribute__ ((availability (macos, introduced = 10.15)))
  __attribute__ ((availability (ios, introduced = 13.0)));

// attribute supplied by a macro
#define AVAILABLE(v) __attribute__ ((availability (macos, introduced = v)))
extern int i6 AVAILABLE (10.15);

// enum
enum __attribute__ ((availability (macos, introduced = 10.5))) E1 { E1_A, E1_B };

#else

extern int i3;
extern int i4;
extern const int i5;
extern int i6;
enum E1 { E1_A, E1_B };

#endif
