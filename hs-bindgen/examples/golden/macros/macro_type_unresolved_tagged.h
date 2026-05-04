// Struct whose parse fails because of an unsupported field type (long double).
// As a result it is absent from the declaration environment, so the macro
// below triggers 'MacroTypecheckErrorUnresolvedTaggedType'.
struct Unparsable {
  long double x;
};

#define PTR_UNPARSABLE struct Unparsable *

#define PTR_DOES_NOT_EXIST struct DoesNotExist *

#define DOES_NOT_EXIST struct DoesNotExist
