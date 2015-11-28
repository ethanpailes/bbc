#ifndef BYTE_BLOCKS__AJEERHIDZUKPYPASAXBI
#define BYTE_BLOCKS__AJEERHIDZUKPYPASAXBI
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
    int32_t f1;
    uint64_t f2;
    uint32_t f3_len;
    int64_t * f3;
} inner;

int inner_size(const inner const * b)
{
    int size = 16;
    size += (b->f3_len * 8);
    return size;
}
int inner_pack(const inner const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int32_t*)(tgt + bytes_written)) = htole32(src->f1); bytes_written += 4;
    *((uint64_t*)(tgt + bytes_written)) = htobe64(src->f2); bytes_written += 8;
    *((uint32_t*)(tgt + bytes_written)) = htobe32(src->f3_len); bytes_written += 4;
    uint32_t f3_iter;
    for(f3_iter = 0; f3_iter < src->f3_len; ++f3_iter) {
        *((int64_t*)(tgt + bytes_written)) = htole64(src->f3[f3_iter]); bytes_written += 8;
    }

    return bytes_written;
}
int inner_unpack_new(inner *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->f1 = le32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->f2 = be64toh(* ((uint64_t*)(src + bytes_consumed))); bytes_consumed += 8;
    uint32_t f3_iter = 0;
    tgt->f3_len = be32toh(* ((uint32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->f3 = malloc(tgt->f3_len * 8);
    for(f3_iter = 0; f3_iter < tgt->f3_len; ++f3_iter) {
        tgt->f3[f3_iter] = le64toh(* ((int64_t*)(src + bytes_consumed))); bytes_consumed += 8;
    }

    return bytes_consumed;
}


int inner_write(const inner *src, FILE *f)
{
    int ret;
    size_t blk_size = inner_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!inner_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int inner_read_new(inner *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
innerRSEQ0:
    if (used + 12 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto innerRSEQ0;
    } else {
        if (fread(buff + used, 12, 1, f) != 1) return false;
        used += 12;
    }
innerRSEQ1a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto innerRSEQ1a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    uint32_t f3_len = be32toh(*( (( uint32_t *) (buff + used)) - 1));
innerRSEQ1b:
    if (used + (f3_len * 8) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto innerRSEQ1b;
    } else {
        if ((f3_len * 8) && fread(buff + used, (f3_len * 8), 1, f) != 1) return false;
        used += (f3_len * 8);
    }
    int ret = inner_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void inner_free(inner *tgt)
{
    free(tgt->f3); tgt->f3 = NULL;
}



typedef struct outer {
    uint8_t f1;
    uint16_t f2_len;
    inner * f2;
} outer;

int outer_size(const outer const * b)
{
    int size = 3;
    uint16_t f2_iter = 0;
    for(f2_iter = 0; f2_iter < b->f2_len; ++f2_iter) {
        size += inner_size(b->f2 + f2_iter);
    }
    return size;
}
int outer_pack(const outer const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((uint8_t*)(tgt + bytes_written)) = (src->f1); bytes_written += 1;
    *((uint16_t*)(tgt + bytes_written)) = htobe16(src->f2_len); bytes_written += 2;
    uint16_t f2_iter;
    for(f2_iter = 0; f2_iter < src->f2_len; ++f2_iter) {
        bytes_written += inner_pack(&(src->f2[f2_iter]), (tgt + bytes_written));
    }

    return bytes_written;
}
int outer_unpack_new(outer *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->f1 = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    uint16_t f2_iter = 0;
    tgt->f2_len = be16toh(* ((uint16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->f2 = malloc(tgt->f2_len * sizeof(inner));
    for(f2_iter = 0; f2_iter < tgt->f2_len; ++f2_iter) {
        bytes_consumed += inner_unpack_new(&(tgt->f2[f2_iter]), (src + bytes_consumed));
    }

    return bytes_consumed;
}


int outer_write(const outer *src, FILE *f)
{
    int ret;
    size_t blk_size = outer_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!outer_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int outer_read_new(outer *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
outerRSEQ0:
    if (used + 1 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto outerRSEQ0;
    } else {
        if (fread(buff + used, 1, 1, f) != 1) return false;
        used += 1;
    }
outerRSEQ1a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto outerRSEQ1a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    uint16_t f2_len = be16toh(*( (( uint16_t *) (buff + used)) - 1));
    uint16_t f2_iter;
    for(f2_iter = 0; f2_iter < f2_len; ++f2_iter) {
    outerRSEQ1inner0:
        if (used + 12 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto outerRSEQ1inner0;
        } else {
            if (fread(buff + used, 12, 1, f) != 1) return false;
            used += 12;
        }
    outerRSEQ1inner1a:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto outerRSEQ1inner1a;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        uint32_t f3_len = be32toh(*( (( uint32_t *) (buff + used)) - 1));
    outerRSEQ1inner1b:
        if (used + (f3_len * 8) > buff_len) {
            grow_buff(&buff, &buff_len);
            goto outerRSEQ1inner1b;
        } else {
            if ((f3_len * 8) && fread(buff + used, (f3_len * 8), 1, f) != 1) return false;
            used += (f3_len * 8);
        }
    }
    int ret = outer_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void outer_free(outer *tgt)
{
    uint16_t f2_iter = 0;
    for(f2_iter = 0; f2_iter < tgt->f2_len; ++f2_iter) {
        inner_free(tgt->f2 + f2_iter);
    }
    free(tgt->f2); tgt->f2 = NULL;
}




#endif
