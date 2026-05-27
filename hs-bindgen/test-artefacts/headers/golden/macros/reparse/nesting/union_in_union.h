#define MyInt int

union { union { MyInt x; }    x; } T1;
union { union { MyInt x; } *  x; } T2;
union { union { MyInt x; } ** x; } T3;
