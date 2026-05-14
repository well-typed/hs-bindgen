//
// Adios and other awkward names
//

// Name with a character that fails the Haskell `isAlphaNum` test
// NOTE: This is not in NFC (it contains a combining diacritical mark).
typedef int adiós;

// We explicitly also test for non-NFC characters in function names, and that we
// can compile the generated Haskell code. See
// https://github.com/well-typed/hs-bindgen/issues/569.
int adiós_fun();

// Type name which starts with a character which
// * is not uppercase
// * cannot be made uppercase
typedef int 数字;

// Function name which starts with a character which
// * is uppercase
// * cannot be made lowercase
// These delightful characters are very rare!
void ϒ(void);

// Like the function ϒ, but now a global variable.
extern int ϒϒ;

// Like the function ϒ, but now a global constant.
extern const int ϒϒϒ;

// Variable name which is valid in both C and Haskell as-is
void 拜拜(void);

// Function name which is contains only "Haskell-valid" characters but starts
// with an uppercase letter
void Say拜拜(void);
