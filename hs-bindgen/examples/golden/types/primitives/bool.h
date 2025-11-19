struct bools1 {
    _Bool x;
    _Bool y;
};

#include <stdbool.h>

struct bools2 {
    bool x;
    bool y;
};

#define BOOL _Bool

struct bools3 {
    BOOL x;
    BOOL y;
};
