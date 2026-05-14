/*
 * Visibility attributes on type declarations
 *
 * https://gcc.gnu.org/onlinedocs/gcc/Common-Attributes.html#index-visibility
 *
 * The example declarations are themselves not terribly interesting.
 * Nonetheless, they are intended to be extensive enough to cover most (if not
 * all) interactions between visibility attributes and the differents ways of
 * declaring struct/union/enum types.
 */

// opaque struct declaration
struct                                            S0;
struct __attribute__ ((visibility ("default")))   S1;
struct __attribute__ ((visibility ("hidden")))    S2;
struct __attribute__ ((visibility ("internal")))  S3;
struct __attribute__ ((visibility ("protected"))) S4;

// struct definition
struct                                            S5 { int x; };
struct __attribute__ ((visibility ("default")))   S6 { int x; };
struct __attribute__ ((visibility ("hidden")))    S7 { int x; };
struct __attribute__ ((visibility ("internal")))  S8 { int x; };
struct __attribute__ ((visibility ("protected"))) S9 { int x; };

// forward declaration with attribute, then a struct definition
struct                                            S10;
struct __attribute__ ((visibility ("default")))   S11;
struct __attribute__ ((visibility ("hidden")))    S12;
struct __attribute__ ((visibility ("internal")))  S13;
struct __attribute__ ((visibility ("protected"))) S14;

struct                                            S10 { int x; };
struct                                            S11 { int x; };
struct                                            S12 { int x; };
struct                                            S13 { int x; };
struct                                            S14 { int x; };

// forward declaration, then a struct definition with attribute
struct                                            S15;
struct                                            S16;
struct                                            S17;
struct                                            S18;
struct                                            S19;

struct                                            S15 { int x; };
struct __attribute__ ((visibility ("default")))   S16 { int x; };
struct __attribute__ ((visibility ("hidden")))    S17 { int x; };
struct __attribute__ ((visibility ("internal")))  S18 { int x; };
struct __attribute__ ((visibility ("protected"))) S19 { int x; };

// opaque union declaration
union                                            U0;
union __attribute__ ((visibility ("default")))   U1;
union __attribute__ ((visibility ("hidden")))    U2;
union __attribute__ ((visibility ("internal")))  U3;
union __attribute__ ((visibility ("protected"))) U4;

// union definition
union                                            U5 { int x; };
union __attribute__ ((visibility ("default")))   U6 { int x; };
union __attribute__ ((visibility ("hidden")))    U7 { int x; };
union __attribute__ ((visibility ("internal")))  U8 { int x; };
union __attribute__ ((visibility ("protected"))) U9 { int x; };

// forward declaration with attribute, then a union definition
union                                            U10;
union __attribute__ ((visibility ("default")))   U11;
union __attribute__ ((visibility ("hidden")))    U12;
union __attribute__ ((visibility ("internal")))  U13;
union __attribute__ ((visibility ("protected"))) U14;

union                                            U10 { int x; };
union                                            U11 { int x; };
union                                            U12 { int x; };
union                                            U13 { int x; };
union                                            U14 { int x; };

// forward declaration, then a union definition with attribute
union                                            U15;
union                                            U16;
union                                            U17;
union                                            U18;
union                                            U19;

union                                            U15 { int x; };
union __attribute__ ((visibility ("default")))   U16 { int x; };
union __attribute__ ((visibility ("hidden")))    U17 { int x; };
union __attribute__ ((visibility ("internal")))  U18 { int x; };
union __attribute__ ((visibility ("protected"))) U19 { int x; };

// opaque enum declaration
enum                                            E0;
enum __attribute__ ((visibility ("default")))   E1;
enum __attribute__ ((visibility ("hidden")))    E2;
enum __attribute__ ((visibility ("internal")))  E3;
enum __attribute__ ((visibility ("protected"))) E4;

// enum definition
enum                                            E5 { x5 };
enum __attribute__ ((visibility ("default")))   E6 { x6 };
enum __attribute__ ((visibility ("hidden")))    E7 { x7 };
enum __attribute__ ((visibility ("internal")))  E8 { x8 };
enum __attribute__ ((visibility ("protected"))) E9 { x9 };

// forward declaration with attribute, then an enum definition
enum                                            E10;
enum __attribute__ ((visibility ("default")))   E11;
enum __attribute__ ((visibility ("hidden")))    E12;
enum __attribute__ ((visibility ("internal")))  E13;
enum __attribute__ ((visibility ("protected"))) E14;

enum                                            E10 { x10 };
enum                                            E11 { x11 };
enum                                            E12 { x12 };
enum                                            E13 { x13 };
enum                                            E14 { x14 };

// forward declaration, then an enum definition with attribute
enum                                            E15;
enum                                            E16;
enum                                            E17;
enum                                            E18;
enum                                            E19;

enum                                            E15 { x15 };
enum __attribute__ ((visibility ("default")))   E16 { x16 };
enum __attribute__ ((visibility ("hidden")))    E17 { x17 };
enum __attribute__ ((visibility ("internal")))  E18 { x18 };
enum __attribute__ ((visibility ("protected"))) E19 { x19 };
