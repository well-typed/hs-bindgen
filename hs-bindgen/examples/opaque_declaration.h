struct foo; /* opaque, no members, no size */
struct foo; /* second declration, still opaque */

struct bar {
    struct foo *ptrA;
    struct bar *ptrB;
};

struct baz {}; /* not opaque */

enum quu;

union opaque_union;
