/*
 * Tentative definitions
 *
 * <https://en.cppreference.com/w/c/language/extern.html#Tentative_definitions>
 */

/* Unlike the extern declarations, which don't change the linkage of an
 * identifier if a previous declaration established it, tentative definitions
 * may disagree in linkage with another declaration of the same identifier. If
 * two declarations for the same identifier are in scope and have different
 * linkage, the behavior is undefined:
 */

static int i4 = 2; // definition, internal linkage
int i4;            // Undefined behavior: linkage disagreement with previous line
extern int i4;     // declaration, refers to the internal linkage definition

static int i5; // tentative definition, internal linkage
int i5;        // Undefined behavior: linkage disagreement with previous line
extern int i5; // refers to previous, whose linkage is internal
