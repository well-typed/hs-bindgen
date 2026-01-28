/*
 * arch_types.c - Implementation of architecture-dependent type examples
 *
 * This file provides implementations for the functions declared in arch_types.h.
 * It's designed to be compilable on any target architecture.
 */

#include "arch_types.h"
#include <stdlib.h>  /* for malloc, NULL */

int get_arch_info_size(void) {
    return (int)sizeof(struct ArchInfo);
}

void init_arch_info(struct ArchInfo *info, long value, int count) {
    if (info == NULL) return;

    info->value = value;
    info->data = NULL;
    info->count = count;
    info->flags = 0;
}

int process_pointer_array(struct PointerArray *arr) {
    if (arr == NULL) return -1;

    int valid_count = 0;
    for (int i = 0; i < arr->length && i < 4; i++) {
        if (arr->items[i] != NULL) {
            valid_count++;
        }
    }
    return valid_count;
}

struct NestedStruct *create_node(long seq) {
    struct NestedStruct *node = (struct NestedStruct *)malloc(sizeof(struct NestedStruct));
    if (node == NULL) return NULL;

    node->info = NULL;
    node->next = NULL;
    node->sequence_number = seq;
    return node;
}
