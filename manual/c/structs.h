/* -------------------------------------------------------------------------- */
/* Nested structures. */

/* Separate declaration of named structure. */
struct door {
  float height;
  float width;
};

/* Use named structure in declaration of nested structure. */
struct room {
  struct door door1;
  struct door door2;
};

/* TODO: https://github.com/well-typed/hs-bindgen/issues/659 */
/* /\* Declare nested structure in an embedded way. The embedded structure is */
/*    anonymous. *\/ */
/* struct aula1 { */
/*   struct { */
/*     float door_height; */
/*     float door_width; */
/*   }; */
/*   int n_doors; */
/* }; */

/* Declare nested structure in an embedded way. The embedded structure has a
   variable name.  */
struct aula2 {
  struct {
    float height;
    float width;
  } door;
  int n_doors;
};

/* -------------------------------------------------------------------------- */
/* Bitfields. */

struct aula_setup {
  char window_id;
  int tilt : 1;
  int close_blinds : 1;
  char projector_id;
  int power_mode : 2;
};

/* -------------------------------------------------------------------------- */
/* Flexible array members. */

struct surname {
  int len;
  char data[];
};
