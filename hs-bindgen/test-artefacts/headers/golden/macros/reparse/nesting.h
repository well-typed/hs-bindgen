#define MyInt int

// reparsing works the same regardless of whether the nested declaration is
// named or unnamed
typedef struct TS1 { MyInt x; } T2;
typedef struct TS3 { MyInt x; } T3;
typedef struct     { MyInt x; } T4;
