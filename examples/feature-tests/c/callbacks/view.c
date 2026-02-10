#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "view.h"

int view_file (const char * __file, view_str view) {
  int fd = open(__file, 0);
  if (fd < 0) {
    printf("Error while opening file: %d\n", fd);
    close(fd);
    return EXIT_FAILURE;
  }
  struct stat statbuf;
  int ret = fstat(fd, &statbuf);
  if (ret < 0) {
    printf("Error while statting file: %d\n", ret);
    close(fd);
    return EXIT_FAILURE;
  }
  size_t file_size = statbuf.st_size;
  void * buf = malloc(file_size);
  int bytes_read = read(fd, buf, file_size);
  if (bytes_read < 0) {
    printf("Error while reading file: %d\n", bytes_read);
    free(buf);
    close(fd);
    return EXIT_FAILURE;
  }
  ret = view((const char *) buf, bytes_read);
  free(buf);
  close(fd);
  return ret;
};

int count_lines (const char str[], size_t str_len) {
  int lines = 0;
  for (int i = 0; i < str_len; i++) {
    if (str[i] == '\n') {
      lines++;
    }
  }
  return lines;
};
