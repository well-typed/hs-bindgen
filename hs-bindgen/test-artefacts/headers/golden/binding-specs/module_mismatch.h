// The prescriptive binding specification (module_mismatch_p.yaml) renames
// `struct point`, but it names another module than the one being generated.
// It is reported once and not used, so the struct keeps its default name.
struct point {
  int x;
  int y;
};
