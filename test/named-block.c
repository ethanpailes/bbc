#include <stdio.h>
#include <unistd.h>
#include <assert.h>


#include "named-block.bb.h"

#define TMP_FILE "/tmp/named-block.dat"

int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");

  inner t;
  t.fieldOne = 34;
  t.fieldTwo = 48;
  inner_write(&t, f);
 
  fclose(f);
  FILE *in = fopen(TMP_FILE, "r");

  inner t2;
  inner_read(&t2, in);

  assert(t2.fieldOne == 34);
  assert(t2.fieldTwo == 48);

  execl("rm", "-f", TMP_FILE, NULL);

  return 0;
}
