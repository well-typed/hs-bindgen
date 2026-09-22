/* A formal parameter may be spelled like a keyword.

   The preprocessor sees pp-tokens, which know no keywords, so within the
   replacement list the parameter shadows every other meaning of its spelling.
   Which spellings libclang reports as keywords depends on the C standard in
   force, and the parse must not.
*/

#define ID_INT(int) int
#define ADD_SIZEOF(sizeof, x) sizeof + x
#define FST_CONST(const, volatile) const
