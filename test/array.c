#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "named-block.bb.h"

#define TMP_FILE "/tmp/named-block.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");

  test t;
  t.f1 = 34;
  t.f2_len = 0;
  outer_write(&t, f);
 
  fclose(f);
  FILE *in = fopen(TMP_FILE, "r");

  outer t2;
  outer_read(&t2, in);

  assert(t2.fieldOne == 34);
  assert(t2.fieldTwo == 48);
  assert(t.nested.fieldOne == 44);
  assert(t.nested.fieldTwo == 9324);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
