/*
 * A declaration cannot introduce an identifier if another declaration for the
 * same identifier in the same scope appears earlier, except that:
 *
 *
 */

// - Declarations of objects with linkage (external or internal) can be repeated:

extern int x;
int x = 10; // OK
extern int x; // OK

static int n;
static int n = 10; // OK
static int n; // OK

// - Non-VLA typedef can be repeated as long as it names the same type:

typedef int int_t;
typedef int int_t; // OK

// - struct and union declarations can be repeated:

struct X;
struct X { int n; };
struct X;

union Y;
union Y { int m; int o; };
union Y;

// These rules simplify the use of header files.
