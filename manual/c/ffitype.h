typedef char A;

typedef A B;

extern B ex_global;

extern B ex_func (B x);

typedef B ex_func_tydef (B x);

typedef B (*ex_func_ptr_tydef) (B x);
