/* Root '#define' directives must reach every C stage: the header libclang
   parses at generation time, and the CAPI wrapper source GHC compiles. */

#if MY_FEATURE
/* Needs the replacement list to be '1', not empty: '#if' on a macro with an
   empty replacement list is an error.  Without the define reaching the
   compilation stage, the wrapper body calls an undeclared function. */
int hash_defines_feature(int x);
#endif

/* Without the define reaching the compilation stage, the header itself does not
   preprocess there. */
struct hash_defines_buffer {
    char data[MY_SIZE];
};

/* Pins the '#define MY_EMPTY' (empty replacement list) rendering. */
#ifdef MY_EMPTY
int hash_defines_empty(void);
#endif
