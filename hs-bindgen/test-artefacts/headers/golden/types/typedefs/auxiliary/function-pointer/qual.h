typedef void (* const T) (int);

/* Pointer to a const function pointer */
typedef void (* const *P) (int);

/* Pointer to an array of const function pointers (const behind an array) */
typedef void (* const (*A)[3]) (int);
