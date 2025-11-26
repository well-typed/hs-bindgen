// This type definition will be parsed (because it is required for scoping), but
// should not be selected!
typedef int ParsedAndNotSelected;

// This declaration is _not_ parsed by the test, which uses a custom parse
// predicate.
struct ParseNotAttemptedNotSelected {
  int x;
};
