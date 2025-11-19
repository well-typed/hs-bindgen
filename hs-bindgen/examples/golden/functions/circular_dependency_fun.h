struct forward_declaration;

typedef void (*fun_ptr)(struct forward_declaration *s);

struct forward_declaration {
  fun_ptr f;
};
