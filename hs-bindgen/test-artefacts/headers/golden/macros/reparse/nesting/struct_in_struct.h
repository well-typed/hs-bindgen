#define MyInt int

struct { struct { MyInt x; }    x; } T1;
struct { struct { MyInt x; } *  x; } T2;
struct { struct { MyInt x; } ** x; } T3;
