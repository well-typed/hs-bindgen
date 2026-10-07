// Names we expect to be escaped: generated modules import them unqualified
// from the Prelude (`hsBindgenReservedNames`)
struct string { int x; };
typedef int Show;
int fmap(int x);

// Names we expect to be escaped: they name types and constructors of
// Foreign.C.Types (`sanityReservedNames`)
struct CChar { int x; };
enum c_types { CInt };

// Names we expect _not_ to be escaped: generated modules import them from the
// Prelude only in another namespace, or not at all
enum constants { Eq, Int, True };
enum ordering { LT, EQ, GT };
typedef int Maybe;
struct Word { int x; };
int reverse(int x);
extern int length;

#define maximum 3
#define twice_maximum (2 * maximum)

// Names we expect _not_ to be escaped: generated modules import them qualified,
// and bind them only as methods of `Read` and `Show` instances
#define readList 1
#define showsPrec (2 * readList)

// Names we expect _not_ to be escaped: generated modules import them qualified
struct Void { int x; };
void *identity(void *ptr);
