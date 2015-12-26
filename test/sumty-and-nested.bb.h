#ifndef BYTE_BLOCKS__RFBASOWDGEBYPQCFLOBS
#define BYTE_BLOCKS__RFBASOWDGEBYPQCFLOBS
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
    int16_t f1;
    int8_t f2_tag;
    union {
        int64_t f2_0;
        uint32_t f2_1;
    } f2;
} inner;

int inner_size(const inner const * b)
{
    int size = 3;
    switch (b->f2_tag) {
    case 0:
      size += 8;
      break;
    case 1:
      size += 4;
      break;
    }
    return size;
}
int inner_pack(const inner const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = htole16(src->f1); bytes_written += 2;
    *((int8_t*)(tgt + bytes_written)) = (src->f2_tag); bytes_written += 1;
    switch (src->f2_tag) {
    case 0:
      *((int64_t*)(tgt + bytes_written)) = htole64(src->f2.f2_0); bytes_written += 8;
      break;
    case 1:
      *((uint32_t*)(tgt + bytes_written)) = htobe32(src->f2.f2_1); bytes_written += 4;
      break;
    }

    return bytes_written;
}
int inner_unpack_new(inner *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->f1 = le16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->f2_tag = (* ((int8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    switch (tgt->f2_tag) {
    case 0:
        tgt->f2.f2_0 = le64toh(* ((int64_t*)(src + bytes_consumed))); bytes_consumed += 8;
        break;
    case 1:
        tgt->f2.f2_1 = be32toh(* ((uint32_t*)(src + bytes_consumed))); bytes_consumed += 4;
        break;
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
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto innerRSEQ0;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
innerRSEQ1a:
    if (used + 1 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto innerRSEQ1a;
    } else {
        if (fread(buff + used, 1, 1, f) != 1) return false;
        used += 1;
    }
    int8_t f2_tag = (*( (( int8_t *) (buff + used)) - 1));
    switch (f2_tag) {
    case 0:
    innerRSEQ1b0:
        if (used + 8 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto innerRSEQ1b0;
        } else {
            if (fread(buff + used, 8, 1, f) != 1) return false;
            used += 8;
        }
        break;
    case 1:
    innerRSEQ1b1:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto innerRSEQ1b1;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        break;
    }
    int ret = inner_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void inner_free(inner *tgt)
{
}



typedef struct outer {
    uint8_t f1;
    int32_t f2;
    uint16_t f3_tag;
    union {
        int32_t f3_0;
        inner f3_1;
    } f3;
} outer;

int outer_size(const outer const * b)
{
    int size = 7;
    switch (b->f3_tag) {
    case 0:
      size += 4;
      break;
    case 1:
        size += inner_size(&(b->f3.f3_1));
      break;
    }
    return size;
}
int outer_pack(const outer const *src, char *tgt)
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
      bytes_written += inner_pack(&(src->f3.f3_1), (tgt + bytes_written));
      break;
    }

    return bytes_written;
}
int outer_unpack_new(outer *tgt, const char const *src)
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
        bytes_consumed += inner_unpack_new(&(tgt->f3.f3_1), (src + bytes_consumed));
        break;
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
    if (used + 5 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto outerRSEQ0;
    } else {
        if (fread(buff + used, 5, 1, f) != 1) return false;
        used += 5;
    }
outerRSEQ1a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto outerRSEQ1a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    uint16_t f3_tag = be16toh(*( (( uint16_t *) (buff + used)) - 1));
    switch (f3_tag) {
    case 0:
    outerRSEQ1b0:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto outerRSEQ1b0;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        break;
    case 1:
        outerRSEQ1inner0:
            if (used + 2 > buff_len) {
                grow_buff(&buff, &buff_len);
                goto outerRSEQ1inner0;
            } else {
                if (fread(buff + used, 2, 1, f) != 1) return false;
                used += 2;
            }
        outerRSEQ1inner1a:
            if (used + 1 > buff_len) {
                grow_buff(&buff, &buff_len);
                goto outerRSEQ1inner1a;
            } else {
                if (fread(buff + used, 1, 1, f) != 1) return false;
                used += 1;
            }
            int8_t f2_tag = (*( (( int8_t *) (buff + used)) - 1));
            switch (f2_tag) {
            case 0:
            outerRSEQ1inner1b0:
                if (used + 8 > buff_len) {
                    grow_buff(&buff, &buff_len);
                    goto outerRSEQ1inner1b0;
                } else {
                    if (fread(buff + used, 8, 1, f) != 1) return false;
                    used += 8;
                }
                break;
            case 1:
            outerRSEQ1inner1b1:
                if (used + 4 > buff_len) {
                    grow_buff(&buff, &buff_len);
                    goto outerRSEQ1inner1b1;
                } else {
                    if (fread(buff + used, 4, 1, f) != 1) return false;
                    used += 4;
                }
                break;
            }
        break;
    }
    int ret = outer_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void outer_free(outer *tgt)
{
    switch (tgt->f3_tag) {
    case 1:
        inner_free(&(tgt->f3.f3_1));
        break;
    default:
        break;
    }
}




#endif
