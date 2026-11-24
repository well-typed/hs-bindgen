#ifndef EXTERNAL_SPECS_USES_EXTERNAL_H
#define EXTERNAL_SPECS_USES_EXTERNAL_H

// These headers lie outside this library directory. The specifications that
// the generated_specs library writes cover their types, so this library uses
// its modules instead of dropping the declarations below.
#include "../generated_specs/same_header.h"
#include "../generated_specs/other_header.h"

struct uses_external {
  same_header a;
  other_header b;
};

struct uses_external uses_external_make(same_header a, other_header b);

#endif
