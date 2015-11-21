#ifndef BYTE_BLOCKS__BAMDBYSIVNCDOWTNVRRH
#define BYTE_BLOCKS__BAMDBYSIVNCDOWTNVRRH
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
    uint16_t f2_len;
    uint32_t * f2;
} test;

inline int test_size(const test const * b)
{
    return 3 + (b->f2_len * 4);
}
int test_pack(const test *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->f1); bytes_written += 1;
    uint16_t f2_iter;
    for(f2_iter = 0; f2_iter < f2_len; ++f2_iter) {
        *((uint32_t*)(tgt + bytes_written)) = htobe32(src->f2[f2_iter]); bytes_written += 4;
    }

    return bytes_written;
}
int test_unpack(test *tgt, const char *src)
{
    tgt->f1 = (* ((uint8_t*)(src + 0)));
TODO HIGHER ORDER TYPES

    return true;
}


int test_write(const test *src, FILE *f)
{
    size_t blk_size = test_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!test_pack(src, buff)) return false;
    fwrite(buff, blk_size, 1, f);
    free(buff);
}
int test_read(test *tgt, FILE *f)
{
TODO var blk len
    if (fread(buff, blk_size, 1, f) != 1) return false;
    return test_unpack(tgt, buff);
}




#endif
