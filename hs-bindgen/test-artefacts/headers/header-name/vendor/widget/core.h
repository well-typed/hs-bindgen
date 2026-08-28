#ifndef HEADER_NAME_VENDOR_CORE_H
#define HEADER_NAME_VENDOR_CORE_H

/* One of three copies of widget/core.h, each in a different search path
   directory. Which one a bracket include finds depends on the -I order, which
   is exactly what the naming has to survive. */
struct header_name_core { int vendor_field; };

#endif
