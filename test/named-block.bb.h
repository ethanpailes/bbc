#ifndef BYTE_BLOCKS__ARENHQJNLOXABKTQKZZZ
#define BYTE_BLOCKS__ARENHQJNLOXABKTQKZZZ
#include <string.h>
#include <stdint.h>
#include <endian.h>
#include <stdlib.h>
#include <stdio.h>

#define true 1
#define false 0

#ifndef __BYTE_BLOCKS_UTILS__
#define __BYTE_BLOCKS_UTILS__
int grow_buff(char ** buff, size_t * len)
{
    size_t old_len = *len;
    char *tmp = *buff;
    *len *=  2;
    *buff = malloc(*len);
    memcpy(*buff, tmp, old_len);
    free(tmp);
    return true;
}
#endif // __BYTE_BLOCKS_UTILS__
typedef struct inner {
    uint8_t fieldOne;
    uint64_t fieldTwo;
} inner;

int inner_size(const inner const * b)
{
    int size = 9;
    return size;
}
int inner_pack(const inner const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->fieldOne); bytes_written += 1;
    *((uint64_t*)(tgt + bytes_written)) = htobe64(src->fieldTwo); bytes_written += 8;

    return bytes_written;
}
int inner_unpack_new(inner *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->fieldOne = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    tgt->fieldTwo = be64toh(* ((uint64_t*)(src + bytes_consumed))); bytes_consumed += 8;

    return bytes_consumed;
}


int inner_write(const inner *src, FILE *f)
{
    int ret;
    size_t blk_size = 9;
    char buff[9];
    if(!inner_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    return ret;
}
int inner_read_new(inner *tgt, FILE *f)
{
    size_t blk_size = 9;
    char buff[blk_size];
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return inner_unpack_new(tgt, buff);
}
void inner_free(inner *tgt)
{
}



typedef struct outer {
    int16_t fieldOne;
    uint32_t fieldTwo;
    inner nested;
} outer;

int outer_size(const outer const * b)
{
    int size = 15;
    return size;
}
int outer_pack(const outer const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = (src->fieldOne); bytes_written += 2;
    *((uint32_t*)(tgt + bytes_written)) = (src->fieldTwo); bytes_written += 4;
    bytes_written += inner_pack(&(src->nested), (tgt + bytes_written));

    return bytes_written;
}
int outer_unpack_new(outer *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->fieldOne = (* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->fieldTwo = (* ((uint32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    bytes_consumed += inner_unpack_new(&(tgt->nested), (src + bytes_consumed));

    return bytes_consumed;
}


int outer_write(const outer *src, FILE *f)
{
    int ret;
    size_t blk_size = 15;
    char buff[15];
    if(!outer_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    return ret;
}
int outer_read_new(outer *tgt, FILE *f)
{
    size_t blk_size = 15;
    char buff[blk_size];
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return outer_unpack_new(tgt, buff);
}
void outer_free(outer *tgt)
{
    inner_free(&(tgt->nested));
}




#endif
