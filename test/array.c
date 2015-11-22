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
  t.f2_len = 4;
  uint32_t arr[] = {4,18,29,3};
  t.f2 = arr;

  test_write(&t, f);
 
  fclose(f);



  FILE *in = fopen(TMP_FILE, "r");

  test t2;
  test_read_new(&t2, in);

  assert(t2.f1 == 34);
  assert(t2.f2_len == 4);
  assert(t2.f2[0] == 4);
  assert(t2.f2[1] == 18);
  assert(t2.f2[2] == 29);
  assert(t2.f2[3] == 3);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
