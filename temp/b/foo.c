int foo() {
  return 0;
}

/* Header & Body */

int __attribute__ ((visibility("default"))) foo_default() {
  return 1;
}

int __attribute__ ((visibility("hidden"))) foo_hidden () {
  return 2;
}

int __attribute__ ((visibility("protected"))) foo_protected() {
  return 3;
}

int __attribute__ ((visibility("internal"))) foo_internal() {
  return 4;
}

/* Body */

int __attribute__ ((visibility("default"))) foo_default_body () {
  return 9;
}

int __attribute__ ((visibility("hidden"))) foo_hidden_body () {
  return 10;
}

int __attribute__ ((visibility("protected"))) foo_protected_body () {
  return 11;
}

int __attribute__ ((visibility("internal"))) foo_internal_body () {
  return 12;
}

/* Override */

int bar() {
  return 13;
}

int bar2() {
  return bar();
}

int __attribute__ ((visibility("protected"))) baz() {
  return 14;
}

int baz2() {
  return baz();
}