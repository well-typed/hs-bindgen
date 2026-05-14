struct Omitted {
  int n;
};

struct DirectlyDependsOnOmitted {
  struct Omitted o;
};

struct IndirectlyDependsOnOmitted {
  struct DirectlyDependsOnOmitted d;
};
