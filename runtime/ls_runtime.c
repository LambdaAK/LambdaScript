/* LambdaScript compiler runtime — linked with clang-generated executables. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Called when a non-exhaustive native [case] falls through (should not happen
    if the program typechecks). */
void ls_abort(void) { abort(); }

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

void *ls_malloc(size_t n) {
  if (n == 0)
    n = 1;
  return malloc(n);
}

/** Tagged sum type: discriminant + optional heap payload pointer. */
typedef struct {
  int32_t tag;
  void *payload;
} ls_variant;

void *ls_variant_mk(int32_t tag, void *payload) {
  ls_variant *p = (ls_variant *)malloc(sizeof(ls_variant));
  if (!p)
    return NULL;
  p->tag = tag;
  p->payload = payload;
  return (void *)p;
}

int32_t ls_variant_tag(void *v) {
  if (!v)
    return -1;
  return ((ls_variant *)v)->tag;
}

void *ls_variant_payload(void *v) {
  if (!v)
    return NULL;
  return ((ls_variant *)v)->payload;
}

void *ls_mkclos(void *code, void *env) {
  struct ls_clos {
    void *code;
    void *env;
  };
  struct ls_clos *p = (struct ls_clos *)malloc(sizeof(struct ls_clos));
  if (!p)
    return NULL;
  p->code = code;
  p->env = env;
  return (void *)p;
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

char *ls_str_concat(const char *a, const char *b) {
  size_t la = a ? strlen(a) : 0;
  size_t lb = b ? strlen(b) : 0;
  char *p = malloc(la + lb + 1);
  if (!p)
    return NULL;
  if (la)
    memcpy(p, a, la);
  if (lb)
    memcpy(p + la, b, lb + 1);
  else
    p[la] = '\0';
  return p;
}
