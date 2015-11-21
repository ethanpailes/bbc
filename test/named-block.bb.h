#ifndef BYTE_BLOCKS__WISEZYSJVERSHCUFYEWT
#define BYTE_BLOCKS__WISEZYSJVERSHCUFYEWT
#include <string.h>
#include <stdint.h>
#ifdef __linux__
#include <endian.h>
#elif defined __APPLE__
#include <machine/endian.h>
#endif
#include <stdio.h>

#define true 1
#define false 0

typedef struct inner {
    uint8_t fieldOne;
    uint64_t fieldTwo;
} inner;

inline int inner_size(const inner const * b)
{
    return 9;
}
int inner_pack(const inner *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->fieldOne); bytes_written += 1;
    *((uint64_t*)(tgt + bytes_written)) = htobe64(src->fieldTwo); bytes_written += 8;

    return bytes_written;
}
int inner_unpack(inner *tgt, const char *src)
{
    tgt->fieldOne = (* ((uint8_t*)(src + 0)));
    tgt->fieldTwo = be64toh(* ((uint64_t*)(src + 1)));

    return true;
}


int inner_write(const inner *src, FILE *f)
{
    size_t blk_size = 9;
    char buff[9];
    if(!inner_pack(src, buff)) return false;
    fwrite(buff, blk_size, 1, f);
}
int inner_read(inner *tgt, FILE *f)
{
    size_t blk_size = 9;
    char buff[blk_size];
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return inner_unpack(tgt, buff);
}



typedef struct outer {
    int16_t fieldOne;
    uint32_t fieldTwo;
    inner nested;
} outer;

inline int outer_size(const outer const * b)
{
    return 15;
}
int outer_pack(const outer *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = (src->fieldOne); bytes_written += 2;
    *((uint32_t*)(tgt + bytes_written)) = (src->fieldTwo); bytes_written += 4;
    bytes_written += inner_pack(&(src->nested), (tgt + bytes_written));

    return bytes_written;
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
    size_t blk_size = 15;
    char buff[15];
    if(!outer_pack(src, buff)) return false;
    fwrite(buff, blk_size, 1, f);
}
int outer_read(outer *tgt, FILE *f)
{
    size_t blk_size = 15;
    char buff[blk_size];
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return outer_unpack(tgt, buff);
}




#endif
