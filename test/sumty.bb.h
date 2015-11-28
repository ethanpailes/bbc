#ifndef BYTE_BLOCKS__TZWIFDNLQEZHUUYAJEHX
#define BYTE_BLOCKS__TZWIFDNLQEZHUUYAJEHX
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
    int32_t f2;
    uint16_t f3_tag;
    union {
        int32_t f3_0;
        uint64_t f3_1;
    } f3;
} test;

int test_size(const test const * b)
{
    int size = 7;
    switch (b->f3_tag) {
    case 0:
      size += 4;
      break;
    case 1:
      size += 8;
      break;
    }
    return size;
}
int test_pack(const test const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->f1); bytes_written += 1;
    *((int32_t*)(tgt + bytes_written)) = htole32(src->f2); bytes_written += 4;
    *((uint16_t*)(tgt + bytes_written)) = htobe16(src->f3_tag); bytes_written += 2;
    switch (src->f3_tag) {
    case 0:
      *((int32_t*)(tgt + bytes_written)) = htole32(src->f3.f3_0); bytes_written += 4;
      break;
    case 1:
      *((uint64_t*)(tgt + bytes_written)) = htobe64(src->f3.f3_1); bytes_written += 8;
      break;
    }

    return bytes_written;
}
int test_unpack_new(test *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->f1 = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    tgt->f2 = le32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->f3_tag = be16toh(* ((uint16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    switch (tgt->f3_tag) {
    case 0:
        tgt->f3.f3_0 = le32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
        break;
    case 1:
        tgt->f3.f3_1 = be64toh(* ((uint64_t*)(src + bytes_consumed))); bytes_consumed += 8;
        break;
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
    if (used + 5 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto testRSEQ0;
    } else {
        if (fread(buff + used, 5, 1, f) != 1) return false;
        used += 5;
    }
testRSEQ1a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto testRSEQ1a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    uint16_t f3_tag = be16toh(*( (( uint16_t *) (buff + used)) - 1));
    switch (f3_tag) {
    case 0:
    testRSEQ1b0:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto testRSEQ1b0;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        break;
    case 1:
    testRSEQ1b1:
        if (used + 8 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto testRSEQ1b1;
        } else {
            if (fread(buff + used, 8, 1, f) != 1) return false;
            used += 8;
        }
        break;
    }
    int ret = test_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void test_free(test *tgt)
{
}




#endif
