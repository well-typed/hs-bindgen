#ifndef VENDOR_DEPENDENCIES_NO_VENDOR_H
#define VENDOR_DEPENDENCIES_NO_VENDOR_H

struct no_vendor {
  const char *name;
  int id;
};

int no_vendor_read(const struct no_vendor *s);

#endif
