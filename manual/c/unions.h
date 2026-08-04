/* -------------------------------------------------------------------------- */
/* Unions */

typedef union occupation {
  struct student {
    char* university;
    int year;
  } student;

  struct employee {
    char* company;
    struct person* supervisor;
    int salary;
  } employee;
} occupation;

void print_occupation(int tag, occupation* o);

/* -------------------------------------------------------------------------- */
/* High-level API generation */

struct person {
  char* name;
  int occupation_tag;

  union occupation occupation;
};

/* -------------------------------------------------------------------------- */
/* Nesting */

// See unions/nesting.h

/* -------------------------------------------------------------------------- */
/* Bit-fields. */

union colour {
  // RGB (Red, Green, Blue)
  struct {
    unsigned R : 10;
    unsigned G : 10;
    unsigned B : 10;
  };

  // CYMK (Cyan, Yellow, Magenta, Black)
  struct {
    unsigned C : 8;
    unsigned Y : 8;
    unsigned M : 8;
    unsigned K : 8;
  };
};

union unforced_width {
  char x;
};

union forced_width {
  long long : 64;
  char x;
};
