#include "selection_squash_into_selected_typedef.h"

struct s {
  int x;
};

// Uses the typedef, which is squashed into the selected struct s, so struct t
// is selected too.
struct t {
  s *ptr;
};
