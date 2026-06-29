#include "unions.h"
#include <stdio.h>

/* -------------------------------------------------------------------------- */
/* Unions */

void print_occupation(int tag, occupation* o) {
    switch(tag) {
        case 0:
            printf("student in year %d\n", o->student.year);
            break;
        case 1:
            printf("employee with salary %d\n", o->employee.salary);
            break;
        default:
            printf("unknown tag\n");
            break;
    };
}
