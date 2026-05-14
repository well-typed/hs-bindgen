/*
 * Attributes on types
 *
 * These examples all come from the gcc manual.
 * https://gcc.gnu.org/onlinedocs/gcc-4.1.0/gcc/Type-Attributes.html
 */

struct S { short f[3]; } __attribute__ ((aligned (8)));
typedef int more_aligned_int __attribute__ ((aligned (8)));

struct S2 { short f[3]; } __attribute__ ((aligned));

struct my_unpacked_struct
{
  char c;
  int i;
};

struct __attribute__ ((__packed__)) my_packed_struct
{
   char c;
   int  i;
   struct my_unpacked_struct s;
};

typedef union
{
  int *__ip;
  union wait *__up;
} wait_status_ptr_t __attribute__ ((__transparent_union__));

typedef int T1 __attribute__ ((deprecated));

typedef short __attribute__((__may_alias__)) short_a;

