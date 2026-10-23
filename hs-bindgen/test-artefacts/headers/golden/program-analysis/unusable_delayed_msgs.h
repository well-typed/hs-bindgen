// Selected declarations that are unusable report their delayed messages

// Both functions get the Haskell name @f@, so neither is usable; @F@ also has
// a delayed parse warning.
void __attribute__ ((visibility ("hidden"))) F (void);
void f (void);
