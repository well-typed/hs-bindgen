// struct with no prior file-scope declaration — prototype-scoped forward ref
void f(struct foo* arg);

// named struct defined inline in prototype — not visible outside
void g(struct foo {int x; char c;}* arg);

// anonymous struct defined inline in prototype — not visible outside
void h(struct {int x; char c;}* arg);
