#pragma once

/* not packed, <=8-bit fields */
union foo_8 {
  signed char a : 3;
  signed char b : 3;
  signed char c : 2;
  signed char d : 3;
  signed char e : 8;
  signed char f : 5;
};

/* not packed, <=16-bit fields */
union foo_16 {
  signed char a :  6;
  signed int  b : 10;
  signed int  c : 16;
  signed int  d : 16;
  signed int  e : 12;
  signed int  f : 12;
};

/* not packed, <=32-bit fields */
union foo_32 {
  signed char a :  6;
  signed int  b : 12;
  signed int  c : 14;
  signed int  d : 10;
  signed long e : 32;
  signed int  f :  6;
  signed long g : 24;
};

/* not packed, <=64-bit fields */
union foo_64 {
  signed long      a : 24;
  signed long long b : 40;
  signed long long c : 64;
  signed long long d : 36;
};

/* packed, <=8-bit fields */
union __attribute__((packed)) foo_8_packed {
  signed char a : 3;
  signed char b : 3;
  signed char c : 2;
  signed char d : 3;
  signed char e : 8;
  signed char f : 5;
};

/* packed, <=16-bit fields */
union __attribute__((packed)) foo_16_packed {
  signed char a :  6;
  signed int  b : 10;
  signed int  c : 16;
  signed int  d : 16;
  signed int  e : 12;
  signed int  f : 12;
};

/* packed, <=32-bit fields */
union __attribute__((packed)) foo_32_packed {
  signed char a :  6;
  signed int  b : 12;
  signed int  c : 14;
  signed int  d : 10;
  signed long e : 32;
  signed int  f :  6;
  signed long g : 24;
};

/* packed, <=64-bit fields */
union __attribute__((packed)) foo_64_packed {
  signed long      a : 24;
  signed long long b : 40;
  signed long long c : 64;
  signed long long d : 36;
};

