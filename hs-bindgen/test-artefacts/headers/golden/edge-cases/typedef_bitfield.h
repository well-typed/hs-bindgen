// Test for issue #1289: typedef bitfield should get Bitfield instance
typedef int myInt;
typedef unsigned int myUInt;
typedef long myLong;

struct myStruct {
  myInt x : 2;
  myUInt y : 4;
  myLong z : 3;
};
