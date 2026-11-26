#ifndef VENDOR_CYCLE_CTX_H
#define VENDOR_CYCLE_CTX_H

#include <vendor/host.h>

// struct lib_ctx and the vendor struct host_api point at each other: with
// program slicing, the vendor module and the types module of ctx.h form an
// import cycle, so each gets a boot file.

struct lib_ctx {
  struct host_api *host;
  int id;
};

void ctx_init(struct lib_ctx *ctx, struct host_api *host);

#endif
