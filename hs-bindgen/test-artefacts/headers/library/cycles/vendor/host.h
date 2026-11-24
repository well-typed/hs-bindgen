#ifndef VENDOR_HOST_H
#define VENDOR_HOST_H

// A vendor header: on the include path, but outside every library directory.
// It declares the library's struct lib_ctx forward.

struct lib_ctx;

struct host_api {
  struct lib_ctx *ctx;
  int (*log)(const char *msg);
};

#endif
