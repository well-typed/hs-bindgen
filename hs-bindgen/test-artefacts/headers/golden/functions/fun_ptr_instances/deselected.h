/* Deselected declarations contribute no instances and no messages */

typedef void V;
typedef int T;

// Selected
void selected(void (*x)(T));

// Deselected: no instances for its argument type
void deselected_eligible(void (*x)(T *));

// Deselected: no warning for its argument type
void deselected_unclassified(V (*x)(void));
