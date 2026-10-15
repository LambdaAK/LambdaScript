/* Forge compiler runtime — linked with clang-generated executables. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/** Report a run-time error (like the interpreter: message on stderr, exit
    status 1) after flushing the program's output. */
static void ls_runtime_error(const char *msg) {
  fflush(stdout);
  fprintf(stderr, "error: runtime error: %s\n", msg);
  exit(1);
}

/** Called when a native [case] (or a refutable [let] pattern) matches no
    branch. */
void ls_abort(void) { ls_runtime_error("no pattern matched"); }

/** Integer division and remainder with OCaml semantics: truncation toward
    zero, division by zero is a run-time error, INT64_MIN / -1 wraps. */
int64_t ls_idiv(int64_t a, int64_t b) {
  if (b == 0)
    ls_runtime_error("division by zero");
  if (b == -1)
    return (int64_t)(0 - (uint64_t)a);
  return a / b;
}

int64_t ls_imod(int64_t a, int64_t b) {
  if (b == 0)
    ls_runtime_error("division by zero");
  if (b == -1)
    return 0;
  return a % b;
}

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
  int64_t tag;
  void *payload;
} ls_variant;

void *ls_variant_mk(int64_t tag, void *payload) {
  ls_variant *p = (ls_variant *)malloc(sizeof(ls_variant));
  if (!p)
    return NULL;
  p->tag = tag;
  p->payload = payload;
  return (void *)p;
}

int64_t ls_variant_tag(void *v) {
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

char *ls_int_to_str(int64_t x) {
  char buf[32];
  int n = snprintf(buf, sizeof buf, "%lld", (long long)x);
  if (n < 0 || n >= (int)sizeof buf)
    return NULL;
  char *p = malloc((size_t)n + 1);
  if (!p)
    return NULL;
  memcpy(p, buf, (size_t)n + 1);
  return p;
}

/* --- Forge Eq / Ord builtins (native compiler) --- */

/** Three-way comparison of floats (structural ==, <, … on Float). */
int64_t ls_cmp_float(double a, double b) { return (a < b) ? -1 : (a > b) ? 1 : 0; }

/** Comparing function values is a run-time error (as in the interpreter). */
void ls_compare_functions(void) { ls_runtime_error("cannot compare functions"); }

int32_t ls_float_eq(double a, double b) { return (a == b) ? 1 : 0; }

void *ls_ordering_int32(int64_t a, int64_t b) {
  int64_t tag = (a < b) ? 0 : (a > b) ? 2 : 1;
  return ls_variant_mk(tag, NULL);
}

void *ls_ordering_char(int64_t a, int64_t b) {
  return ls_ordering_int32(a, b);
}

void *ls_ordering_str(const char *a, const char *b) {
  int c = strcmp(a ? a : "", b ? b : "");
  int64_t tag = (c < 0) ? 0 : (c > 0) ? 2 : 1;
  return ls_variant_mk(tag, NULL);
}

void *ls_ordering_bool(int8_t xa, int8_t xb) {
  int64_t a = xa ? 1 : 0;
  int64_t b = xb ? 1 : 0;
  return ls_ordering_int32(a, b);
}

void *ls_ordering_float(double a, double b) {
  int64_t tag;
  if (a < b)
    tag = 0;
  else if (a > b)
    tag = 2;
  else
    tag = 1;
  return ls_variant_mk(tag, NULL);
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

/* --- String and conversion builtins (native compiler) --- */

int64_t ls_str_length(const char *s) { return s ? (int64_t)strlen(s) : 0; }

/** [str_slice s start len], clamped to the string like the interpreter. */
char *ls_str_slice(const char *s, int64_t start, int64_t len) {
  int64_t n = s ? (int64_t)strlen(s) : 0;
  if (start < 0)
    start = 0;
  if (start > n)
    start = n;
  if (len < 0)
    len = 0;
  if (len > n - start)
    len = n - start;
  char *p = malloc((size_t)len + 1);
  if (!p)
    return NULL;
  if (len > 0)
    memcpy(p, s + start, (size_t)len);
  p[len] = '\0';
  return p;
}

/** List cell for a [List<Char>] (layout matches the compiler's
    [{ i64, i8* }] cons cells). */
typedef struct ls_char_cell {
  int64_t head;
  struct ls_char_cell *tail;
} ls_char_cell;

void *ls_string_to_list(const char *s) {
  ls_char_cell *head = NULL;
  int64_t n = s ? (int64_t)strlen(s) : 0;
  for (int64_t i = n - 1; i >= 0; i--) {
    ls_char_cell *c = malloc(sizeof(ls_char_cell));
    if (!c)
      return NULL;
    c->head = (unsigned char)s[i];
    c->tail = head;
    head = c;
  }
  return head;
}

double ls_int_to_float(int64_t x) { return (double)x; }

/* Float arithmetic ([+.] [-.] [*.] [/.]) and [sqrt], [exp], [log]: IEEE
   doubles, like the interpreter's OCaml floats. */
double ls_fadd(double a, double b) { return a + b; }
double ls_fsub(double a, double b) { return a - b; }
double ls_fmul(double a, double b) { return a * b; }
double ls_fdiv(double a, double b) { return a / b; }
double ls_sqrt(double x) { return sqrt(x); }
double ls_exp(double x) { return exp(x); }
double ls_log(double x) { return log(x); }

/** Truncation toward zero (like OCaml's int_of_float for finite values). */
int64_t ls_float_to_int(double f) {
  if (f != f)
    return 0;
  if (f >= 9.2233720368547758e18)
    return INT64_MAX;
  if (f <= -9.2233720368547758e18)
    return INT64_MIN;
  return (int64_t)f;
}

/** [float_to_str]: the shortest %g form that reads back as the same double,
    with ".0" added to integral values (same strings as the interpreter). */
char *ls_float_to_str(double f) {
  char buf[64];
  if (f != f)
    snprintf(buf, sizeof buf, "nan");
  else if (f == 1.0 / 0.0)
    snprintf(buf, sizeof buf, "inf");
  else if (f == -1.0 / 0.0)
    snprintf(buf, sizeof buf, "-inf");
  else {
    /* At least as many digits as the integer part has (-10.0, not -1e+01). */
    int start = 1;
    double a = f < 0 ? -f : f;
    if (a >= 1 && a < 1e16)
      for (; a >= 10; a /= 10)
        start++;
    for (int p = start; p <= 17; p++) {
      snprintf(buf, sizeof buf, "%.*g", p, f);
      if (strtod(buf, NULL) == f)
        break;
    }
    if (!strpbrk(buf, ".e"))
      strcat(buf, ".0");
  }
  size_t n = strlen(buf);
  char *p = malloc(n + 1);
  if (!p)
    return NULL;
  memcpy(p, buf, n + 1);
  return p;
}

char *ls_char_to_str(int64_t c) {
  char *p = malloc(2);
  if (!p)
    return NULL;
  p[0] = (char)c;
  p[1] = '\0';
  return p;
}

/** Ordering (LT | EQ | GT) from a three-way comparison result. */
void *ls_ordering_of_cmp(int64_t c) {
  return ls_variant_mk(c < 0 ? 0 : (c > 0 ? 2 : 1), NULL);
}
