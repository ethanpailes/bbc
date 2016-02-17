#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "fixed-array.bb.h"

#define TMP_FILE "/tmp/bbc-fixed-array.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");
  
  fixed_array_test t;
  t.f1 = 34;
  t.f2[0] = 5578;
  t.f2[31] = 8863;
  t.f2[2] = 2904;
  t.f2[4] = 3345;
  t.f2[30] = 404;
  t.f2[9] = 9;

  fixed_array_test_write(&t, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  fixed_array_test t2;
  fixed_array_test_read_new(&t2, in);

  assert(t2.f1 == 34);
  assert(t2.f2[0] == 5578);
  assert(t2.f2[31] == 8863);
  assert(t2.f2[2] == 2904);
  assert(t2.f2[4] == 3345);
  assert(t2.f2[30] == 404);
  assert(t2.f2[9] == 9);

  fixed_array_test_free(&t2);

  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
