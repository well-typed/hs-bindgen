// The types module of these bindings has a boot file and a callback module.
// The callback module has no boot file.
//
// This is the only golden fixture whose test case turns on --generate-hs-boot,
// so the fixture compile checks a boot file against its module only here. The
// header therefore has one declaration of each kind that the boot file of a
// types module treats in its own way.

#include <stdint.h>

// A callback: the callback module has the ToFunPtr and FromFunPtr instances
// for On_event_Aux, and the "wrapper" and "dynamic" imports behind them. The
// types module keeps the newtype.
typedef int (*on_event)(int code);

// A struct with a flexible array member: the types module has a type synonym,
// Buffer, which its boot file gives in full.
struct buffer {
  int len;
  char data[];
};

void register_handler(on_event f);

struct point {
  int x;
  int y;
};

// A callback written in place, whose type names a type of the module: the
// callback module has the ToFunPtr and FromFunPtr instances at the function
// type Ptr Point -> IO CInt, and the "wrapper" and "dynamic" imports behind
// them.
void visit(int (*f)(struct point *p));

// An enum: a newtype, abstract in the boot file, which leaves out its pattern
// synonyms and COMPLETE pragma.
enum colour { RED, GREEN, BLUE };

// A union: a newtype too.
union value {
  int i;
  float f;
};

// A typedef of a primitive type, and a typedef of a typedef that the stdlib
// binding specification binds to HsBindgen.Runtime.LibC.Int32.
typedef int status;
typedef int32_t sample;

// A macro type, and a macro value, whose type signature the boot file gives.
#define COUNT_T unsigned int
#define MAX_EVENTS 16

// An opaque struct: data without constructors, in the module as well.
struct handle;
