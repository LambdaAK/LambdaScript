/* LambdaScript compiler runtime — linked with clang-generated executables. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ls_print(const char *s) {
  if (!s)
    return;
  fputs(s, stdout);
}

void ls_println(const char *s) {
  if (!s) {
    putchar('\n');
    return;
  }
  puts(s);
}

char *ls_int_to_str(int32_t x) {
  char buf[32];
  int n = snprintf(buf, sizeof buf, "%d", (int)x);
  if (n < 0 || n >= (int)sizeof buf)
    return NULL;
  char *p = malloc((size_t)n + 1);
  if (!p)
    return NULL;
  memcpy(p, buf, (size_t)n + 1);
  return p;
}
