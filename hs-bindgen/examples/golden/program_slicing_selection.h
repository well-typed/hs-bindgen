#include <errno.h>  /* ENOENT, EACCES, ... */
#include <stddef.h> /* size_t */
#include <stdint.h> /* uint32_t, uint64_t */
#include <stdio.h>  /* FILE */
#include <time.h>   /* time_t */

enum FileOperationStatus {
  SUCCESS = 0,
  NOT_FOUND = ENOENT,
  PERMISSION_DENIED = EACCES,
  INVALID_ARGUMENT = EINVAL,
  OUT_OF_MEMORY = ENOMEM,
  CUSTOM_ERROR_OTHER = -1
};

struct FileOperationRecord {
  enum FileOperationStatus status;
  size_t bytes_processed;
};

enum FileOperationStatus read_file_chunk(FILE *file_ptr, void *buffer,
                                         size_t bytes_to_read);

struct UnselectedStruct {
  time_t time_t_should_not_be_in_fixtures;
};
