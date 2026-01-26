// Multiple uses of anon decl by declaring multiple typedefs
// https://github.com/well-typed/hs-bindgen/issues/1430

// Case 1: Both direct typedefs
typedef struct { int x; int y; } point1a, point1b;

// Case 2: One direct, one pointer
typedef struct { int x; int y; } point2a, *point2b;

// Case 3: Both pointers
typedef struct { int x; int y; } *point3a, *point3b;

// Verify that point2a and point2b are compatible types
void test(point2a x, point2b y) {
    *y = x;
}
