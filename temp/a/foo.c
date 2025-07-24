int foo(int x) {
  return x*x;
}

int __attribute__ ((visibility("default"))) foo_default(int x) {
  return foo(x);
}

int __attribute__ ((visibility("hidden"))) foo_hidden (int x) {
  return foo(x);
}

int __attribute__ ((visibility("protected"))) foo_protected(int x) {
  return foo(x);
}

int __attribute__ ((visibility("internal"))) foo_internal(int x) {
  return foo(x);
}