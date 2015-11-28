#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "sumty-and-nested.bb.h"

#define TMP_FILE "/tmp/bbc-sumty-and-nested.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");

  outer t1, t2;
 
  t1.f1 = 78;
  t1.f2 = -999;
  t1.f3_tag = 0;
  t1.f3.f3_0 = 17788;

  t2.f1 = 78;
  t2.f2 = -999;
  t2.f3_tag = 1;

  inner i;
  i.f1 = 167;
  i.f2_tag = 0;
  i.f2.f2_0 = 1678;

  t2.f3.f3_1 = i;

  outer_write(&t1, f);
  outer_write(&t2, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  outer t3, t4;
  outer_read_new(&t3, in);
  outer_read_new(&t4, in);

  assert(t3.f1 == 78);
  assert(t3.f2 == -999);
  assert(t3.f3_tag == 0);
  assert(t3.f3.f3_0 == 17788);

  assert(t4.f1 == 78);
  assert(t4.f2 == -999);
  assert(t4.f3_tag == 1);

  inner i1 = t4.f3.f3_1;
  assert(i1.f1 == 167);
  assert(i1.f2_tag == 0);
  assert(i1.f2.f2_0 == 1678);

  outer_free(&t3);
  outer_free(&t4);

  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
