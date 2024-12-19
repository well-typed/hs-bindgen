#define OBJECTLIKE1 1
#define OBJECTLIKE2 ( 2 )
#define OBJECTLIKE3 3 + 3
#define OBJECTLIKE4 ( 4 + 4 )

#define MEANING_OF_LIFE1 42
#define MEANING_OF_LIFE2 052
#define MEANING_OF_LIFE3 0x2a
#define MEANING_OF_LIFE4 0X2A
#define MEANING_OF_LIFE5 0b101010

#define LONG_INT_TOKEN1 18446744073709550592ull
#define LONG_INT_TOKEN2 18'446'744'073'709'550'592llu
#define LONG_INT_TOKEN3 1844'6744'0737'0955'0592uLL
#define LONG_INT_TOKEN4 184467'440737'0'95505'92LLU

#define TUPLE1 ( 1 , 2 )
#define TUPLE2 3 , 4
#define TUPLE3 5, 6

// ---
// https://en.cppreference.com/w/cpp/language/floating_literal
// (1)
#define FLT1_1 11e4
#define FLT1_2 12E-3
#define FLT1_3 13e-03f
// (2)
#define FLT2_1 21.
#define FLT2_2 22.e2
#define FLT2_3 23.f
// (3)
#define FLT3_1 31.0
#define FLT3_2 .32
#define FLT3_3 .33e2
#define FLT3_4 .34e-2f
// (4)
#define FLT4_1 0x41p4
#define FLT4_2 0x42P-3
#define FLT4_3 0x43p-03f
// (5)
#define FLT5_1 0x51.p0
#define FLT5_2 0x52.P0f
// (6)
#define FLT6_1 0x61.0P2
#define FLT6_2 0x.62p2
#define FLT6_3 0x.63p-2f
// ---

#define BAD1 0.1 + 1
#define BAD2 2l * 2ul
