#pragma once

#define MkInnerType(t)\
  t {\
    int fieldX;\
    char fieldY;\
  }\

#define MkOuterType(t1, n1, t2)\
  typedef t1 n1 {\
    MkInnerType(t2);\
  } n1;\


MkOuterType(struct, SS, struct)
MkOuterType(struct, SU, union)
MkOuterType(union,  US, struct)
MkOuterType(union,  UU, union)

typedef struct {
  int fieldX;
  char fieldY;
} str_repr;

#define MkShowFunName(t1) show_ ## t1
#define MkShowFun(t1)\
  static str_repr MkShowFunName(t1) (t1 obj) {\
    str_repr ret = { .fieldX=obj.fieldX, .fieldY=obj.fieldY };\
    return ret;\
  }

MkShowFun(SS)
MkShowFun(SU)
MkShowFun(US)
MkShowFun(UU)
