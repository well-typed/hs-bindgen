#include "pt.h"

// Underlying types named (only) by typedefs that are given the emptydata
// representation in staticsize.h.  They live in a separate header so program
// slicing keeps them out of the generated output.

typedef struct pt pt_alias;

union shape { int tag; double value; };
