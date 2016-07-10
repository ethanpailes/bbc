#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "array.bb.h"

#define TMP_FILE "/tmp/bbc-large-array.dat"

#define ARRAY_LEN 8192


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");
  

  int i;
  test t;
  t.f1 = 34;
  t.f2_len = ARRAY_LEN;
  t.f2 = malloc(sizeof(*t.f2) * ARRAY_LEN);
  for (i = 0; i < ARRAY_LEN; ++i)
    t.f2[i] = i;

  test_write(&t, f);
 
  fclose(f);
  free(t.f2);

  FILE *in = fopen(TMP_FILE, "r");

  test t2;
  test_read_new(&t2, in);

  assert(t2.f1 == 34);
  assert(t2.f2_len == ARRAY_LEN);

  for (i = 0; i < ARRAY_LEN; ++i)
    assert(t2.f2[i] == i);

  test_free(&t2);

  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
