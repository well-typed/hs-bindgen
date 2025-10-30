/**
 * Attributes on functions
 *
 * Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html
 */

typedef struct {} FILE;
typedef int size_t;

// access (access-mode, ref-index)
// access (access-mode, ref-index, size-index)
// SKIPPED (unsupported by clang)

// alias ("target")

static void __f1 () {
  return;
}
void f1 () __attribute__ ((weak, alias ("__f1")));

// alloc_align (position)

void* my_memalign (size_t, size_t) __attribute__ ((alloc_align (1)));

// alloc_size (position)
// alloc_size (position-1, position-2)

void* my_calloc (size_t, size_t) __attribute__ ((alloc_size (1, 2)));
void* my_realloc (void*, size_t) __attribute__ ((alloc_size (2)));

// assume_aligned (alignment)
// assume_aligned (alignment, offset)

void* my_alloc1 (size_t) __attribute__((assume_aligned (16)));
void* my_alloc2 (size_t) __attribute__((assume_aligned (32, 8)));

// const

int square (int) __attribute__ ((const));

// copy
// copy (function)
// SKIPPED (unsupported by clang)

// deprecated
// deprecated (msg)

int old_fn_deprecated () __attribute__ ((deprecated("Use new_function instead")));

// unavailable
// unavailable (msg)

int old_fn_unavailable () __attribute__((unavailable("Use new_function instead")));

// format (archetype, string-index, first-to-check)

extern int
my_printf (void *my_object, const char *my_format, ...)
      __attribute__ ((format (printf, 2, 3)));

// format_arg (string-index)

extern char *
my_dgettext (char *my_domain, const char *my_format)
      __attribute__ ((format_arg (2)));

// SKIPPED: ifunc ("resolver")

// malloc
// malloc (deallocator)
// malloc (deallocator, ptr-index)
// SKIPPED: only "malloc" with no arguments is supported by clang.

__attribute__ ((malloc))
  FILE* fdopen (int, const char*);

// no_sanitize ("sanitize_option")

void __attribute__ ((no_sanitize ("alignment", "object-size"))) f2 ();

// nonnull
// nonnull (arg-index, …)

extern void *
my_memcpy (void *dest, const void *src, size_t len)
        __attribute__((nonnull (1, 2)));

extern void *
my_memcpy (void *dest, const void *src, size_t len)
        __attribute__((nonnull));

// nonnull_if_nonzero
// nonnull_if_nonzero (arg-index, arg2-index)
// nonnull_if_nonzero (arg-index, arg2-index, arg3-index)
// SKIPPED: unsupported by clang

// noplt
// SKIPPED: unsupported by clang

// noreturn

void fatal () __attribute__ ((noreturn));

// null_terminated_string_arg
// null_terminated_string_arg (N)
// SKIPPED: unsupported by clang

// pure

int hash (char *) __attribute__ ((pure));

// returns_nonnull

extern void *
mymalloc (size_t len) __attribute__((returns_nonnull));

// section ("section-name")

extern void foobar (void) __attribute__ ((section ("bar")));

// symver ("name2@nodename") ¶
// SKIPPED: unsupported by clang

// target (string, …)

int core2_func (void) __attribute__ ((__target__ ("arch=core2")));
int sse3_func (void) __attribute__ ((__target__ ("sse3")));

// visibility ("visibility_type")

void __attribute__ ((visibility ("protected"))) f3 ();
int i __attribute__ ((visibility ("hidden")));

// warn_unused_result

int fn () __attribute__ ((warn_unused_result));

// weakref
// weakref ("target")

/* Given the declaration: */
extern int y (void);

/* the following... */
static int x1 (void) __attribute__ ((weakref ("y")));

/* is equivalent to... */
static int x2 (void) __attribute__ ((weakref, alias ("y")));
