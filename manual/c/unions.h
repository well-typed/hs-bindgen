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
