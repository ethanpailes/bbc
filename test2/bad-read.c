#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "bad-read.bb.h"

#define TMP_FILE "/tmp/bbc-bad-read.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");
  
  testone t;
  t.f1 = 34;
  t.f2_len = 2;
  uint32_t arr[] = {4, 18};
  t.f2 = arr;

  testone_write(&t, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  testtwo t2;
  assert(!testtwo_read_new(&t2, in));

  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
