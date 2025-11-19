/*
 * Declaration of a macro of the same name as an existing global variable
 *
 * Found this case in the wild in <stdio.h>
 */

typedef int FILE;

extern FILE *const stdin;
extern FILE *const stdout;
extern FILE *const stderr;

#define stdin  (stdin)
#define stdout (stdout)
#define stderr (stderr)
