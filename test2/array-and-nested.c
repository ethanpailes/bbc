#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "array-and-nested.bb.h"

#define TMP_FILE "/tmp/bbc-array-and-nested.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");
 
  inner i1;
  i1.f1 = -89;
  i1.f2 = 99;
  int64_t arr1[] = {4,18,29,3};
  i1.f3_len = 4;
  i1.f3 = arr1;

  inner i2;
  i2.f1 = -48;
  i2.f2 = 23;
  int64_t arr2[] = {9,5,-14,100};
  i2.f3_len = 4;
  i2.f3 = arr2;


  outer o;
  o.f1 = 9;
  o.f2_len = 2;
  inner arr3[2];
  arr3[0] = i1;
  arr3[1] = i2;
  o.f2 = arr3;


  outer_write(&o, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  outer t;
  outer_read_new(&t, in);

  assert(t.f1 == 9);
  assert(t.f2_len == 2);
  assert(t.f2[0].f1 == -89);
  assert(t.f2[0].f2 == 99);
  assert(t.f2[0].f3_len == 4);
  assert(t.f2[0].f3[0] == 4);
  assert(t.f2[0].f3[1] == 18);
  assert(t.f2[0].f3[2] == 29);
  assert(t.f2[0].f3[3] == 3);

  assert(t.f2[1].f1 == -48);
  assert(t.f2[1].f2 == 23);
  assert(t.f2[1].f3_len == 4);
  assert(t.f2[1].f3[0] == 9);
  assert(t.f2[1].f3[1] == 5);
  assert(t.f2[1].f3[2] == -14);
  assert(t.f2[1].f3[3] == 100);

  outer_free(&t);

  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
