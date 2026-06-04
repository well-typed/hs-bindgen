#define MyInt int

struct { union { MyInt x; }    x; } T1;
struct { union { MyInt x; } *  x; } T2;
struct { union { MyInt x; } ** x; } T3;
