// For all the global variables below we should generate a pure Haskell binding
// that simply returns the value, since all global variables are (indirectly
// through a typedef) const-qualified.
//
// In the past, we had a bug where the const qualifier was lost in the typedef
// examples if the typedef wrapped a const-qualified struct, union or enum (but
// not basic types like int), see PR #1488. This golden test should hopefully
// help us catch regressions were they to appear in the future.

typedef int I;
struct S {};
union U {};
enum E {foo};

extern const I i;
extern const struct S s;
extern const union U u;
extern const enum E e;

typedef const I TI;
typedef const struct S TS;
typedef const union U TU;
typedef const enum E TE;

extern TI ti;
extern TS ts;
extern TU tu;
extern TE te;

typedef TI TTI;
typedef TS TTS;
typedef TU TTU;
typedef TE TTE;

extern TTI tti;
extern TTS tts;
extern TTU ttu;
extern TTE tte;
