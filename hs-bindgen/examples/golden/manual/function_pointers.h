/*
 * Function pointers
 */

extern int square(int);

extern int plus(int, int);

extern int apply1 (int (*f)(int), int x);

extern int apply2 (int (*f)(int, int), int x, int y);

/*
 * Implicit function to pointer conversion
 *
 * See the "Functions" section of the manual for more information.
 */

typedef int int2int(int);

//! Basically the same as apply1(), but here for illustratory purposes.
extern int apply1_pointer_arg (int2int *, int);

//! A version of apply1_pointer_arg() that declares to take a argument of
//! function type, rather than a pointer-to-function type.
extern int apply1_nopointer_arg (int2int, int);

/* Parameters of function type can occur almost anywhere! */

//! A function returning a pointer to a function like apply1_nopointer().
extern int (* const apply1_nopointer_res (void)) (int2int, int);

//! A global variable pointing to a function like apply1_nopointer().
extern int (* const apply1_nopointer_var) (int2int, int);

//! A struct field pointing to a function like apply1_nopointer().
struct Apply1Struct {
  int (* const apply1_nopointer_struct_field)(int2int, int);
};
extern const struct Apply1Struct apply1_struct;

//! A union field pointing to a function like apply1_nopointer().
union Apply1Union {
  int (* const apply1_nopointer_union_field)(int2int, int);
};
extern const union Apply1Union apply1_union;
