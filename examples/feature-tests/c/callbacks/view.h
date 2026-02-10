#pragma once

#include <stddef.h>

// View file contents in one go
typedef char str[];
typedef int (*view_str)(const str str, size_t str_len);

extern int view_file (const char * __file, view_str view);

extern int count_lines (const str str, size_t str_len);
// view_str count_lines would be nice
