#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>


#include "fixed-array-sumty.bb.h"

#define TMP_FILE "/tmp/bbc-fixed-array-nested.dat"


int main(int argc, char *argv[])
{
  FILE *f = fopen(TMP_FILE, "w");


  outer o1;
  
  o1.f2_tag = 2; // inner flag

  o1.f2.f2_2.f3 = 19;
  o1.f2.f2_2.f4[8] = 12;

  o1.f2.f2_2.f5_tag = 2; // test2 flag
  o1.f2.f2_2.f5.f5_2.f9 = 18;
  o1.f2.f2_2.f5.f5_2.f10[0] = 4;
  o1.f2.f2_2.f5.f5_2.f10[511] = 12312;
  o1.f2.f2_2.f5.f5_2.f10[5] = 121;


  outer_write(&o1, f);
 
  fclose(f);

  FILE *in = fopen(TMP_FILE, "r");

  outer o2;

  outer_read_new(&o2, in);

  assert(o1.f2_tag == 2);

  assert(o1.f2.f2_2.f3 == 19);
  assert(o1.f2.f2_2.f4[8] == 12);

  assert(o1.f2.f2_2.f5_tag == 2);
  assert(o1.f2.f2_2.f5.f5_2.f9 == 18);
  assert(o1.f2.f2_2.f5.f5_2.f10[0] == 4);
  assert(o1.f2.f2_2.f5.f5_2.f10[511] == 12312);
  assert(o1.f2.f2_2.f5.f5_2.f10[5] == 121);


  fclose(in);

  execl("rm", "rm", "-f", TMP_FILE, NULL);

  return 0;
}
