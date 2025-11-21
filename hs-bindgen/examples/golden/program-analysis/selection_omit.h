struct Omitted {
  int n;
};

struct DependsOnOmitted {
  struct Omitted o;
};
