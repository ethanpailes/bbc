#ifndef BYTE_BLOCKS__OTNSSRVWCFEJXGBJVDYF
#define BYTE_BLOCKS__OTNSSRVWCFEJXGBJVDYF
#include <string.h>
#include <stdint.h>
#include <endian.h>
#include <stdio.h>

#define true 1
#define false 0

typedef struct inner {
    uint8_t fieldOne;
    uint64_t fieldTwo;
} inner;

int inner_pack(const inner *src, char *tgt)
{
    *((uint8_t*)(tgt + 0)) = (src->fieldOne);
    *((uint64_t*)(tgt + 1)) = htobe64(src->fieldTwo);

    return true;
}
int inner_unpack(inner *tgt, const char *src)
{
    tgt->fieldOne = (* ((uint8_t*)(src + 0)));
    tgt->fieldTwo = be64toh(* ((uint64_t*)(src + 1)));

    return true;
}


int inner_write(const inner *src, FILE *f)
{
    char buff[9];
    if(!inner_pack(src, buff)) return false;
    fwrite(buff, 9, 1, f);
}
int inner_read(inner *tgt, FILE *f)
{
    char buff[9];
    memset(buff, 0, 9);

    if (fread(buff, 9, 1, f) != 1) return false;
    return inner_unpack(tgt, buff);
}



typedef struct outer {
    int16_t fieldOne;
    uint32_t fieldTwo;
    inner nested;
} outer;

int outer_pack(const outer *src, char *tgt)
{
    *((int16_t*)(tgt + 0)) = (src->fieldOne);
    *((uint32_t*)(tgt + 2)) = (src->fieldTwo);
    inner_pack(&(src->nested), (tgt + 6));

    return true;
}
int outer_unpack(outer *tgt, const char *src)
{
    tgt->fieldOne = (* ((int16_t*)(src + 0)));
    tgt->fieldTwo = (* ((uint32_t*)(src + 2)));
    inner_unpack(&(tgt->nested), (src + 6));

    return true;
}


int outer_write(const outer *src, FILE *f)
{
    char buff[9];
    if(!outer_pack(src, buff)) return false;
    fwrite(buff, 9, 1, f);
}
int outer_read(outer *tgt, FILE *f)
{
    char buff[9];
    memset(buff, 0, 9);

    if (fread(buff, 9, 1, f) != 1) return false;
    return outer_unpack(tgt, buff);
}




#endif
