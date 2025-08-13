
/*
 * A definition is a declaration that provides all information about the identifiers it declares.
 *
 * Every declaration of an enum or a typedef is a definition.
 *
 * <https://en.cppreference.com/w/c/language/declarations.html#Definitions>
 */

// For functions, a declaration that includes the function body is a function definition:

int foo(double); // declaration
int foo(double x) { return x; } // definition

// For objects, a declaration that allocates storage (automatic or static, but not extern) is a definition, while a declaration that does not allocate storage (external declaration) is not.

extern int n; // declaration
int n = 10; // definition

// For structs and unions, declarations that specify the list of members are definitions:

struct X; // declaration
struct X { int n; }; // definition

union Y; // declaration
union Y { int m; int o; }; // definition
