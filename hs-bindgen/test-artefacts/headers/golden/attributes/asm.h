// We should ignore assembler labels.
extern int asm_labeled_variable asm("my_variable_label");

int asm_labeled_function(int x, int y) asm("my_function_label");
