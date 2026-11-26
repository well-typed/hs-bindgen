#ifndef GENERATED_SPECS_SAME_HEADER_H
#define GENERATED_SPECS_SAME_HEADER_H

// The typedef is squashed into the struct of the same header.
typedef struct same_header {
  double value;
} same_header;

// The prescriptive binding specification omits this struct. The generated
// specification of this header records the omission.
struct omitted {
  double x;
};

double same_header_get(same_header s);

#endif
