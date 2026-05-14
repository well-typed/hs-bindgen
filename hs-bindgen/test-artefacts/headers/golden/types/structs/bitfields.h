#ifndef TEST_BITFIELDS_H
#define TEST_BITFIELDS_H

/* not packed, <=8-bit fields */
struct foo_8 {
  signed char a : 3;
  signed char b : 3;
  signed char c : 2;  // First byte fully packed
  signed char d : 3;  // Middle byte has padding
  signed char e : 8;  // 8-bit field
  signed char f : 5;  // Last byte has padding
};

/* not packed, <=16-bit fields */
struct foo_16 {
  signed char a :  6;
  signed int  b : 10;  // First two bytes fully packed
  signed int  c : 16;  // 16-bit field
  signed int  d : 16;
  signed int  e : 12;  // Middle byte has padding
  signed int  f : 12;  // Padding at end
};

/* not packed, <=32-bit fields */
struct foo_32 {
  signed char a :  6;
  signed int  b : 12;
  signed int  c : 14;  // First four bytes fully packed
  signed int  d : 10;  // Middle byte has padding
  signed long e : 32;  // 32-bit field
  signed int  f :  6;
  signed long g : 24;  // Padding at end
};

/* not packed, <=64-bit fields */
struct foo_64 {
  signed long      a : 24;
  signed long long b : 40;  // First eight bytes fully packed
  signed long long c : 64;  // 64-bit field
  signed long long d : 36;  // Padding at end
};

/* packed, <=8-bit field crosses 8-bit word boundary */
struct __attribute__((packed)) bar_8_8 {
  signed char a : 6;
  signed int  b : 4;  // Crosses 8-bit word boundary
};

/* packed, <=8-bit field crosses 16-bit word boundary */
struct __attribute__((packed)) bar_8_16 {
  signed int a : 14;
  signed int b :  4;  // Crosses 16-bit word boundary
};

/* packed, <=8-bit field crosses 32-bit word boundary */
struct __attribute__((packed)) bar_8_32 {
  signed long a : 30;
  signed int  b :  4;  // Crosses 32-bit word boundary
};

/* packed, <=8-bit field crosses 64-bit word boundary */
struct __attribute__((packed)) bar_8_64 {
  signed long long a : 62;
  signed int       b :  4;  // Crosses 64-bit word boundary
};

/* packed, <=16-bit field crosses 16-bit word boundary */
struct __attribute__((packed)) bar_16_16 {
  signed int a : 14;
  signed int b : 14;  // Crosses 16-bit word boundary
};

/* packed, <=16-bit field crosses 32-bit word boundary */
struct __attribute__((packed)) bar_16_32 {
  signed long a : 24;
  signed int  b : 14;  // Crosses 32-bit word boundary
};

/* packed, <=16-bit field crosses 64-bit word boundary */
struct __attribute__((packed)) bar_16_64 {
  signed long long a : 56;
  signed int       b : 14;  // Crosses 64-bit word boundary
};

/* packed, <=32-bit field crosses 32-bit word boundary */
struct __attribute__((packed)) bar_32_32 {
  signed long a : 30;
  signed long b : 30;  // Crosses 32-bit word boundary
};

/* packed, <=32-bit field crosses 64-bit word boundary */
struct __attribute__((packed)) bar_32_64 {
  signed long long a : 56;
  signed long      b : 30;  // Crosses 64-bit word boundary
};

/* packed, <=64-bit field crosses 64-bit word boundary */
struct __attribute__((packed)) bar_64_64 {
  signed long long a : 56;
  signed long long b : 40;  // Crosses 64-bit word boundary
};

#endif // TEST_BITFIELDS_H
