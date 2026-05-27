#define MyInt int

union { struct { MyInt x; }    x; } T1;
union { struct { MyInt x; } *  x; } T2;
union { struct { MyInt x; } ** x; } T3;
