/*
 * The flag_enum and enum_extensibility attributes
 *
 * https://clang.llvm.org/docs/AttributeReference.html#flag-enum
 * https://clang.llvm.org/docs/AttributeReference.html#enum-extensibility
 *
 * Both attributes only affect compiler warnings. The generated bindings are
 * the same as for an enum without them. In particular, the pattern synonyms of
 * an enum marked as closed are not declared COMPLETE.
 */

// flag_enum, in both spellings
enum __attribute__ ((flag_enum))     E0 { E0_A = 1, E0_B = 2 };
enum __attribute__ ((__flag_enum__)) E1 { E1_A = 1, E1_B = 2 };

// enum_extensibility
enum __attribute__ ((enum_extensibility (open)))   E2 { E2_A, E2_B };
enum __attribute__ ((enum_extensibility (closed))) E3 { E3_A, E3_B };

// both attributes
enum __attribute__ ((enum_extensibility (open),   flag_enum)) E4 { E4_A = 1, E4_B = 2 };
enum __attribute__ ((enum_extensibility (closed), flag_enum)) E5 { E5_A = 1, E5_B = 2 };
