#pragma once

/**
 * Some simple macros
 */

#define FIELD_OFFSET 4
#define EPSILON 0.1

#define PTR_TO_FIELD(ptr) ptr + 4

#define YEAR int
#define MONTH int
#define DAY int

typedef struct date {
  YEAR year;
  MONTH month;
  DAY day;
} date;

YEAR getYear(date *d);

/**
 * Character and string literals
 *
 * Object-like macros that expand to a single character or string literal.
 */

// A character literal. Its value fits in a single byte.
#define LETTER 'a'

// A character literal given as an escape sequence. '\a' is the ASCII BEL
// control character, which resolves to the single (non-printable) byte 7.
#define BELL '\a'

// A string literal. `hs-bindgen` represents this as the execution-encoding
// bytes of the literal (assuming a UTF-8 execution charset).
#define GREETING "hello"

// A string literal with non-ASCII characters ("hello" in Japanese). With a
// UTF-8 execution charset each character is stored as several bytes, so the
// byte length (15) exceeds the number of characters (5).
#define GREETING_JP "こんにちは"

// Return the length of a NUL-terminated string (a small `strlen`).
int greeting_length(char *str);

#define CHINESE 'c'

#define JAPANESE 'j'

// Greet in Chinese ('c') or Japanese ('j').
void greet(char lang);
