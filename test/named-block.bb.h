#ifndef BYTE_BLOCKS__DSKMEDBRDVXJWTKUFSNL
#define BYTE_BLOCKS__DSKMEDBRDVXJWTKUFSNL
#include <string.h>
#include <stdint.h>
#include <endian.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct inner {
    uint8_t fieldOne;
    uint64_t fieldTwo;
} inner;


int inner_read(inner *tgt, FILE *f)
{
    uint8_t buff[9];
    memset(buff, 0, 9);

    if (fread(buff, 9, 1, f) != 1) return false;

    tgt->fieldOne = (* ((uint8_t*)(buff + 0)));
    tgt->fieldTwo = be64toh(* ((uint64_t*)(buff + 1)));

    return true;
}

int inner_write(inner *src, FILE *f)
{
    uint8_t buff[9];

    *((uint8_t*)(buff + 0)) = (src->fieldOne);
    *((uint64_t*)(buff + 1)) = htobe64(src->fieldTwo);

    if (fwrite(buff, 9, 1, f) != 1) return false;

    return true;
}


typedef struct outer {
    int16_t fieldOne;
    uint32_t fieldTwo;
    struct inner nested;
} outer;


int outer_read(outer *tgt, FILE *f)
{
    uint8_t buff[9];
    memset(buff, 0, 9);

    if (fread(buff, 9, 1, f) != 1) return false;

    tgt->fieldOne = (* ((int16_t*)(buff + 0)));
    tgt->fieldTwo = (* ((uint32_t*)(buff + 2)));
    inner_read(&(tgt->nested), f);

    return true;
}

int outer_write(outer *src, FILE *f)
{
    uint8_t buff[9];

    *((int16_t*)(buff + 0)) = (src->fieldOne);
    *((uint32_t*)(buff + 2)) = (src->fieldTwo);
    inner_write(&(src->nested), f);

    if (fwrite(buff, 9, 1, f) != 1) return false;

    return true;
}



#endif
