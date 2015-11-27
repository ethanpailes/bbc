#ifndef BYTE_BLOCKS__QHJPYYFEFXCZXLOMTQTG
#define BYTE_BLOCKS__QHJPYYFEFXCZXLOMTQTG
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
typedef struct test {
    uint8_t f1;
    uint8_t f2;
    int32_t f3;
    int64_t f4;
    uint16_t f6;
} test;

int test_size(const test const * b)
{
    int size = 16;
    return size;
}
int test_pack(const test const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->f1); bytes_written += 1;
    *((uint8_t*)(tgt + bytes_written)) = (src->f2); bytes_written += 1;
    *((int32_t*)(tgt + bytes_written)) = (src->f3); bytes_written += 4;
    *((int64_t*)(tgt + bytes_written)) = htobe64(src->f4); bytes_written += 8;
    *((uint16_t*)(tgt + bytes_written)) = htole16(src->f6); bytes_written += 2;

    return bytes_written;
}
int test_unpack_new(test *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->f1 = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    tgt->f2 = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    tgt->f3 = (* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->f4 = be64toh(* ((int64_t*)(src + bytes_consumed))); bytes_consumed += 8;
    tgt->f6 = le16toh(* ((uint16_t*)(src + bytes_consumed))); bytes_consumed += 2;

    return bytes_consumed;
}


int test_write(const test *src, FILE *f)
{
    int ret;
    size_t blk_size = 16;
    char buff[16];
    if(!test_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    return ret;
}
int test_read_new(test *tgt, FILE *f)
{
    size_t blk_size = 16;
    char buff[blk_size];
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return test_unpack_new(tgt, buff);
}
void test_free(test *tgt)
{
}




#endif
