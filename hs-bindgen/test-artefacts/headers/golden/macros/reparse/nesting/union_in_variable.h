#define MyInt int

union { MyInt x; }    G1;
union { MyInt x; } *  G2;
union { MyInt x; } ** G3;
