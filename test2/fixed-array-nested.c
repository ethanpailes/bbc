#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "fixed-array-nested.bb.h"

#define TMP_FILE "/tmp/bbc-fixed-array-nested.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");


  outer o1;
  
  o1.f1 = 198;

  o1.f2[0].f3 = 3290;
  o1.f2[0].f4[0] = 354;
  o1.f2[0].f4[99] = 3242;
  o1.f2[0].f4[255] = 19021;
  o1.f2[0].f5 = 330;

  o1.f2[3].f3 = 90;
  o1.f2[3].f4[0] = 35;
  o1.f2[3].f4[30] = 342;
  o1.f2[3].f4[255] = 1901;
  o1.f2[3].f5 = 3;

  outer_write(&o1, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  outer o2;

  outer_read_new(&o2, in);

  assert(o2.f1 == 198);

  assert(o2.f2[0].f3 == 3290);
  assert(o2.f2[0].f4[0] == 354);
  assert(o2.f2[0].f4[99] == 3242);
  assert(o2.f2[0].f4[255] == 19021);
  assert(o2.f2[0].f5 == 330);

  assert(o2.f2[3].f3 == 90);
  assert(o2.f2[3].f4[0] == 35);
  assert(o2.f2[3].f4[30] == 342);
  assert(o2.f2[3].f4[255] == 1901);
  assert(o2.f2[3].f5 == 3);


  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
