#include <stdio.h>
#include <unistd.h>
#include <assert.h>


#include "aligned-bitfields.bb.h"

#define TMP_FILE "/tmp/aligned-bitfields.dat"

int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");

  test t;
  t.f1 = 34;
  t.f6 = 48;
  t.f4 = -9834;
  test_write(&t, f);
 
  fclose(f);
  FILE *in = fopen(TMP_FILE, "r");

  test t2;
  test_read(&t2, in);

  assert(t2.f1 == 34);
  assert(t2.f6 == 48);
  assert(t2.f4 == -9834);

  execl("rm", "-f", TMP_FILE, NULL);

  return 0;
}
