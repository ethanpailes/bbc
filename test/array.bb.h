#ifndef BYTE_BLOCKS__CWHPXNJOTFBYVQSVDFWI
#define BYTE_BLOCKS__CWHPXNJOTFBYVQSVDFWI
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
    uint16_t f2_len;
    uint32_t * f2;
} test;

int test_size(const test const * b)
{
    int size = 3;
    size += (b->f2_len * 4);
    return size;
}
int test_pack(const test const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->f1); bytes_written += 1;
    *((uint16_t*)(tgt + bytes_written)) = htobe16(src->f2_len); bytes_written += 2;
    uint16_t f2_iter;
    for(f2_iter = 0; f2_iter < src->f2_len; ++f2_iter) {
        *((uint32_t*)(tgt + bytes_written)) = htobe32(src->f2[f2_iter]); bytes_written += 4;
    }

    return bytes_written;
}
int test_unpack_new(test *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->f1 = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    uint16_t f2_iter = 0;
    tgt->f2_len = be16toh(* ((uint16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->f2 = malloc(tgt->f2_len * 4);
    for(f2_iter = 0; f2_iter < tgt->f2_len; ++f2_iter) {
        tgt->f2[f2_iter] = be32toh(* ((uint32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    }

    return bytes_consumed;
}


int test_write(const test *src, FILE *f)
{
    int ret;
    size_t blk_size = test_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!test_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int test_read_new(test *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
testRSEQ0:
    if (used + 1 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto testRSEQ0;
    } else {
        if (fread(buff + used, 1, 1, f) != 1) return false;
        used += 1;
    }
testRSEQ1a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto testRSEQ1a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    uint16_t f2_len = be16toh(*( (( uint16_t *) (buff + used)) - 1));
testRSEQ1b:
    if (used + (f2_len * 4) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto testRSEQ1b;
    } else {
        if ((f2_len * 4) && fread(buff + used, (f2_len * 4), 1, f) != 1) return false;
        used += (f2_len * 4);
    }
    int ret = test_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void test_free(test *tgt)
{
    free(tgt->f2); tgt->f2 = NULL;
}




#endif
