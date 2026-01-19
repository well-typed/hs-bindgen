struct Vector {
  int length;
  long numbers[];
};

struct Vector *vector_alloc(int n);

void vector_free(struct Vector *v);

void vector_reverse(struct Vector *v);
