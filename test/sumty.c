#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "sumty.bb.h"

#define TMP_FILE "/tmp/bbc-sumty.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");

  test t1, t2;
  t1.f1 = 34;
  t1.f2 = -196;
  t1.f3_tag = 0;
  t1.f3.f3_0 = -78;

  t2.f1 = 43;
  t2.f2 = -16;
  t2.f3_tag = 1;
  t2.f3.f3_1 = 7899;

  test_write(&t1, f);
  test_write(&t2, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  test t3, t4;
  test_read_new(&t3, in);
  test_read_new(&t4, in);

  assert(t3.f1 == 34);
  assert(t3.f2 == -196);
  assert(t3.f3_tag == 0);
  assert(t3.f3.f3_0 == -78);

  assert(t4.f1 == 43);
  assert(t4.f2 == -16);
  assert(t4.f3_tag == 1);
  assert(t4.f3.f3_1 == 7899);

  test_free(&t3);
  test_free(&t4);

  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
