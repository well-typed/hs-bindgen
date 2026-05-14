typedef struct linked_list_A_s {
  int x;
  struct linked_list_A_s* next;
} linked_list_A_t;

// forward declaration, to avoid two names
struct linked_list_B_t;
typedef struct linked_list_B_t linked_list_B_t;
struct linked_list_B_t {
  int x;
  linked_list_B_t* next;
};
