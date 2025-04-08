// Variations on the examples in macro_in_fundecl, with typedef'd args
// TODO: Test the other variants in `macro_in_fundecl`.

#define MC char
typedef char TC;

// very basic
char quux1(MC x, TC y) { return 0; }
TC quux2(MC x, char y) { return 0; }
MC *wam1(float x, TC *y) { return 0; }
TC *wam2(float x, MC *y) { return 0; }

// structs
// 1. tag, no typedef
// 2. no tag, typedef
// 3. tag and typedef, but different
// 4. tag and typedef, identical
        struct struct1 { int a; };
typedef struct         { int a; } struct2;
typedef struct struct3 { int a; } struct3_t;
typedef struct struct4 { int a; } struct4;

void struct_typedef1(struct2   *s, MC x);
void struct_typedef2(struct3_t *s, MC x);
void struct_typedef3(struct4   *s, MC x);

// TODO https://github.com/well-typed/hs-bindgen/issues/549
// Reparser does not understand `struct <name>` syntax yet.
// void struct_name1(struct struct1 *s, MC x);
// void struct_name2(struct struct3 *s, MC x);
// void struct_name3(struct struct4 *s, MC x);