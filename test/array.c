#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "array.bb.h"

#define TMP_FILE "/tmp/bbc-array.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");

  test t;
  t.f1 = 34;
  t.f2_len = 0;
  test_write(&t, f);
 
  fclose(f);
  FILE *in = fopen(TMP_FILE, "r");

  test t2;
  test_read(&t2, in);

  assert(t2.f1 == 34);
  assert(t2.f2_len == 0);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
