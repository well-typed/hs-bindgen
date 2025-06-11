//
// Adios and other awkward names
//

// Name with a character that fails the Haskell `isAlphaNum` test
// NOTE: This is not in NFC (it contains a combining diacritical mark).
typedef int adiós;

// Type name which starts with a character which
// * is not uppercase
// * cannot be made uppercase
typedef int 数字;

// Variable name which starts with a character which
// * is uppercase
// * cannot be made lowercase
// These delightful characters are very rare!
void ϒ(void);

// Variable name which is valid in both C and Haskell as-is
void 拜拜(void);

// Function name which is contains only "Haskell-valid" characters but starts
// with an uppercase letter
void Say拜拜(void);



