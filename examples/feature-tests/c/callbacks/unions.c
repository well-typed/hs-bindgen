#include "unions.h"

Val increment_object (Val v, Typ t) {
  switch (t) {
    case LongLong:
      v.integer++;
      break;
    case Float:
      v.floating++;
      break;
  }
  return v;
}

Val apply_object_op (object_op * f, Val v, Typ t) {
  return (*f)(v, t);
}
