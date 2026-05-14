/*
 * Tentative definitions
 *
 * <https://en.cppreference.com/w/c/language/extern.html#Tentative_definitions>
 */

/*
 * A tentative definition is an external declaration without an initializer, and
 * either without a storage-class specifier or with the specifier static.
 *
 * A tentative definition is a declaration that may or may not act as a
 * definition. If an actual external definition is found earlier or later in the
 * same translation unit, then the tentative definition just acts as a
 * declaration.
 */

int i1 = 1;     // definition, external linkage
int i1;         // tentative definition, acts as declaration because i1 is defined
extern int i1;  // declaration, refers to the earlier definition

extern int i2 = 3; // definition, external linkage
int i2;            // tentative definition, acts as declaration because i2 is defined
extern int i2;     // declaration, refers to the external linkage definition

/*
 * If there are no definitions in the same translation unit, then the tentative
 * definition acts as an actual definition that empty-initializes the object.
 */

int i3;        // tentative definition, external linkage
int i3;        // tentative definition, external linkage
extern int i3; // declaration, external linkage
 // in this translation unit, i3 is defined as if by "int i3 = 0;"
