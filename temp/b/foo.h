int foo();

/* Header & Body */

int foo_default() __attribute__ ((visibility("default")));

int foo_hidden () __attribute__ ((visibility("hidden")));

int foo_protected () __attribute__ ((visibility("protected")));

int foo_internal () __attribute__ ((visibility("internal")));

/* Header */

int __attribute__ ((visibility("default"))) foo_default_header () {
  return 5;
}

int __attribute__ ((visibility("hidden"))) foo_hidden_header () {
  return 6;
}

int __attribute__ ((visibility("protected"))) foo_protected_header () {
  return 7;
}

int __attribute__ ((visibility("internal"))) foo_internal_header () {
  return 8;
}

/* Override */

int bar2();

int baz2();
