// The types module of these bindings has an HsBootInternal module, and both
// modules have boot files.

// A callback: the types module has ToFunPtr and FromFunPtr instances for
// On_event_Aux, and the functions that they name are in the HsBootInternal
// module.
typedef int (*on_event)(int code);

// A struct with a flexible array member: the types module has a type synonym,
// Buffer, which its boot file gives in full.
struct buffer {
  int len;
  char data[];
};

void register_handler(on_event f);
