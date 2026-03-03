/* callbacks with unions (by value) */

typedef enum { LongLong, Float } Typ;

typedef union {
  long long integer;
  float floating;
} Val;

typedef Val object_op (Val v, Typ t);

extern Val increment_object (Val v, Typ t);

extern Val apply_object_op (object_op * f, Val v, Typ t);
