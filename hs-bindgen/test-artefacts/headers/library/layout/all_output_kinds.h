// Root header of the library. It lies outside the library directory, so it
// gets no module of its own.
//
// The run writes every kind of file library mode generates: a types module
// with its Safe, Unsafe, FunPtr and Global modules, boot files, a per-header
// Callback module, the global Callback module and binding specifications.
#include "all_output_kinds/nested/by_value.h"
#include "all_output_kinds/cycle_a.h"
#include "all_output_kinds/cycle_b.h"
#include "all_output_kinds/acyclic_callback.h"
