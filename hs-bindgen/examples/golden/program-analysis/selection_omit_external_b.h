// Includes the root header, and uses the omitted declaration.
#include "selection_omit_external_root.h"

struct DirectlyDependsOnOmitted {
  struct Omitted o;
};

struct IndirectlyDependsOnOmitted {
  struct DirectlyDependsOnOmitted d;
};
