#ifndef BYTE_BLOCKS__LQZXMUKBLLGDVZBNJITN
#define BYTE_BLOCKS__LQZXMUKBLLGDVZBNJITN
#include <string.h>
#include <stdint.h>
#include <endian.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct test {
    uint8_t  f1;
    uint8_t  f2;
    int32_t  f3;
    int64_t  f4;
    uint16_t  f6;
} test;

int test_read(test *tgt, FILE *f)
{
    uint8_t buff[16];
    memset(buff, 0, 16);

    if (fread(buff, 16, 1, f) != 1) return false;

    tgt->f1 = (* ((uint8_t*)(buff + 0)));
    tgt->f2 = (* ((uint8_t*)(buff + 1)));
    tgt->f3 = (* ((int32_t*)(buff + 2)));
    tgt->f4 = be64toh(* ((int64_t*)(buff + 6)));
    tgt->f6 = le16toh(* ((uint16_t*)(buff + 14)));

    return true;
}

int test_write(test *src, FILE *f)
{
    uint8_t buff[16];

    *((uint8_t*)(buff + 0)) = (src->f1);
    *((uint8_t*)(buff + 1)) = (src->f2);
    *((int32_t*)(buff + 2)) = (src->f3);
    *((int64_t*)(buff + 6)) = htobe64(src->f4);
    *((uint16_t*)(buff + 14)) = htole16(src->f6);

    if (fwrite(buff, 16, 1, f) != 1) return false;

    return true;
}
#endif
