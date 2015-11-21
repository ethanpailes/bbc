#ifndef BYTE_BLOCKS__LOAYLZYOZXMDRYVURURW
#define BYTE_BLOCKS__LOAYLZYOZXMDRYVURURW
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

typedef struct test {
    uint8_t f1;
    uint8_t f2;
    int32_t f3;
    int64_t f4;
    uint16_t f6;
} test;

inline int test_size(const test const * b)
{
    return 16;
}
int test_pack(const test *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->f1); bytes_written += 1;
    *((uint8_t*)(tgt + bytes_written)) = (src->f2); bytes_written += 1;
    *((int32_t*)(tgt + bytes_written)) = (src->f3); bytes_written += 4;
    *((int64_t*)(tgt + bytes_written)) = htobe64(src->f4); bytes_written += 8;
    *((uint16_t*)(tgt + bytes_written)) = htole16(src->f6); bytes_written += 2;

    return bytes_written;
}
int test_unpack(test *tgt, const char *src)
{
    tgt->f1 = (* ((uint8_t*)(src + 0)));
    tgt->f2 = (* ((uint8_t*)(src + 1)));
    tgt->f3 = (* ((int32_t*)(src + 2)));
    tgt->f4 = be64toh(* ((int64_t*)(src + 6)));
    tgt->f6 = le16toh(* ((uint16_t*)(src + 14)));

    return true;
}


int test_write(const test *src, FILE *f)
{
    size_t blk_size = 16;
    char buff[16];
    if(!test_pack(src, buff)) return false;
    fwrite(buff, blk_size, 1, f);
}
int test_read(test *tgt, FILE *f)
{
    size_t blk_size = 16;
    char buff[blk_size];
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return test_unpack(tgt, buff);
}




#endif
