#include <stdio.h>

/* -------------------------------------------------------------------------- */
/* Structures with and without typedef. */

typedef struct triple {
  int a;
  int b;
  int c;
} triple;

typedef struct triple triple_t;

void mk_triple(int a, int b, int c, triple *triple);

/* -------------------------------------------------------------------------- */
/* Nesting */

// See structs/nesting.h

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
  size_t len;
  char data[];
};

struct surname *surname_alloc(const char nm[]);

void surname_free(struct surname *ptr);

/* -------------------------------------------------------------------------- */
/* Opaque. */

struct square;

struct square *create_square(double side_length);
