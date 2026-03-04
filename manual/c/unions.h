#include <stdio.h>

/* -------------------------------------------------------------------------- */
/* Nested unions. */

/* Separate declaration of named union. */
union length_unit {
  int feet;
  int meter;
};

/* Use named union in declaration of nested union. */
union dimension {
  union length_unit length;
  union length_unit width;
  union length_unit height;
};

/* Declare nested union in an embedded way. The embedded union has a variable
   name. */
union length1 {
  union {
    int feet;
    int meter;
  } feet_or_meter;
  int yard;
};

/* Declare nested union in an embedded way. The embedded union is anonymous. */
union length2 {
  union {
    int feet;
    int meter;
  };
  int yard;
};
