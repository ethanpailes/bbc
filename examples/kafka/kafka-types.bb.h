#ifndef BYTE_BLOCKS__JDJEMDQNAHHVLYJYKGYK
#define BYTE_BLOCKS__JDJEMDQNAHHVLYJYKGYK
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
typedef struct wrapper {
    int32_t msg_len;
    uint8_t * msg;
} wrapper;

int wrapper_size(const wrapper const * b)
{
    int size = 4;
    size += (b->msg_len * 1);
    return size;
}
int wrapper_pack(const wrapper const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->msg_len); bytes_written += 4;
    int32_t msg_iter;
    for(msg_iter = 0; msg_iter < src->msg_len; ++msg_iter) {
        *((uint8_t*)(tgt + bytes_written)) = (src->msg[msg_iter]); bytes_written += 1;
    }

    return bytes_written;
}
int wrapper_unpack_new(wrapper *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    int32_t msg_iter = 0;
    tgt->msg_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->msg = malloc(tgt->msg_len * 1);
    if (tgt->msg_len < 0) tgt->msg_len = 0;
    for(msg_iter = 0; msg_iter < tgt->msg_len; ++msg_iter) {
        tgt->msg[msg_iter] = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    }

    return bytes_consumed;
}


int wrapper_write(const wrapper *src, FILE *f)
{
    int ret;
    size_t blk_size = wrapper_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!wrapper_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int wrapper_read_new(wrapper *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
wrapperRSEQ0a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto wrapperRSEQ0a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t msg_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
wrapperRSEQ0b:
    if (used + (msg_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto wrapperRSEQ0b;
    } else {
        if ((msg_len * 1) && fread(buff + used, (msg_len * 1), 1, f) != 1) return false;
        used += (msg_len * 1);
    }
    int ret = wrapper_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void wrapper_free(wrapper *tgt)
{
    free(tgt->msg); tgt->msg = NULL;
}



typedef struct bytes {
    int16_t b_len;
    uint8_t * b;
} bytes;

int bytes_size(const bytes const * b)
{
    int size = 2;
    size += (b->b_len * 1);
    return size;
}
int bytes_pack(const bytes const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = htobe16(src->b_len); bytes_written += 2;
    int16_t b_iter;
    for(b_iter = 0; b_iter < src->b_len; ++b_iter) {
        *((uint8_t*)(tgt + bytes_written)) = (src->b[b_iter]); bytes_written += 1;
    }

    return bytes_written;
}
int bytes_unpack_new(bytes *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    int16_t b_iter = 0;
    tgt->b_len = be16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->b = malloc(tgt->b_len * 1);
    if (tgt->b_len < 0) tgt->b_len = 0;
    for(b_iter = 0; b_iter < tgt->b_len; ++b_iter) {
        tgt->b[b_iter] = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    }

    return bytes_consumed;
}


int bytes_write(const bytes *src, FILE *f)
{
    int ret;
    size_t blk_size = bytes_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!bytes_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int bytes_read_new(bytes *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
bytesRSEQ0a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto bytesRSEQ0a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    int16_t b_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
bytesRSEQ0b:
    if (used + (b_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto bytesRSEQ0b;
    } else {
        if ((b_len * 1) && fread(buff + used, (b_len * 1), 1, f) != 1) return false;
        used += (b_len * 1);
    }
    int ret = bytes_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void bytes_free(bytes *tgt)
{
    free(tgt->b); tgt->b = NULL;
}



typedef struct string {
    int16_t s_len;
    uint8_t * s;
} string;

int string_size(const string const * b)
{
    int size = 2;
    size += (b->s_len * 1);
    return size;
}
int string_pack(const string const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = htobe16(src->s_len); bytes_written += 2;
    int16_t s_iter;
    for(s_iter = 0; s_iter < src->s_len; ++s_iter) {
        *((uint8_t*)(tgt + bytes_written)) = (src->s[s_iter]); bytes_written += 1;
    }

    return bytes_written;
}
int string_unpack_new(string *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    int16_t s_iter = 0;
    tgt->s_len = be16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->s = malloc(tgt->s_len * 1);
    if (tgt->s_len < 0) tgt->s_len = 0;
    for(s_iter = 0; s_iter < tgt->s_len; ++s_iter) {
        tgt->s[s_iter] = (* ((uint8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    }

    return bytes_consumed;
}


int string_write(const string *src, FILE *f)
{
    int ret;
    size_t blk_size = string_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!string_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int string_read_new(string *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
stringRSEQ0a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto stringRSEQ0a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
stringRSEQ0b:
    if (used + (s_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto stringRSEQ0b;
    } else {
        if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
        used += (s_len * 1);
    }
    int ret = string_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void string_free(string *tgt)
{
    free(tgt->s); tgt->s = NULL;
}



typedef struct request_message_hdr {
    int16_t api_key;
    int16_t api_version;
    int32_t correlation_id;
    string client_id;
} request_message_hdr;

int request_message_hdr_size(const request_message_hdr const * b)
{
    int size = 8;
    size += string_size(&(b->client_id));
    return size;
}
int request_message_hdr_pack(const request_message_hdr const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = htobe16(src->api_key); bytes_written += 2;
    *((int16_t*)(tgt + bytes_written)) = htobe16(src->api_version); bytes_written += 2;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->correlation_id); bytes_written += 4;
    bytes_written += string_pack(&(src->client_id), (tgt + bytes_written));

    return bytes_written;
}
int request_message_hdr_unpack_new(request_message_hdr *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->api_key = be16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->api_version = be16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->correlation_id = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    bytes_consumed += string_unpack_new(&(tgt->client_id), (src + bytes_consumed));

    return bytes_consumed;
}


int request_message_hdr_write(const request_message_hdr *src, FILE *f)
{
    int ret;
    size_t blk_size = request_message_hdr_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!request_message_hdr_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int request_message_hdr_read_new(request_message_hdr *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
request_message_hdrRSEQ0a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto request_message_hdrRSEQ0a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
request_message_hdrRSEQ0b:
    if (used + (s_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto request_message_hdrRSEQ0b;
    } else {
        if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
        used += (s_len * 1);
    }
    int ret = request_message_hdr_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void request_message_hdr_free(request_message_hdr *tgt)
{
    string_free(&(tgt->client_id));
}



typedef struct metadata_request {
    int32_t topics_len;
    string * topics;
} metadata_request;

int metadata_request_size(const metadata_request const * b)
{
    int size = 4;
    int32_t topics_iter = 0;
    for(topics_iter = 0; topics_iter < b->topics_len; ++topics_iter) {
        size += string_size(b->topics + topics_iter);
    }
    return size;
}
int metadata_request_pack(const metadata_request const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->topics_len); bytes_written += 4;
    int32_t topics_iter;
    for(topics_iter = 0; topics_iter < src->topics_len; ++topics_iter) {
        bytes_written += string_pack(&(src->topics[topics_iter]), (tgt + bytes_written));
    }

    return bytes_written;
}
int metadata_request_unpack_new(metadata_request *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    int32_t topics_iter = 0;
    tgt->topics_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->topics = malloc(tgt->topics_len * sizeof(string));
    if (tgt->topics_len < 0) tgt->topics_len = 0;
    for(topics_iter = 0; topics_iter < tgt->topics_len; ++topics_iter) {
        bytes_consumed += string_unpack_new(&(tgt->topics[topics_iter]), (src + bytes_consumed));
    }

    return bytes_consumed;
}


int metadata_request_write(const metadata_request *src, FILE *f)
{
    int ret;
    size_t blk_size = metadata_request_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!metadata_request_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int metadata_request_read_new(metadata_request *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
metadata_requestRSEQ0a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto metadata_requestRSEQ0a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t topics_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
    int32_t topics_iter;
    for(topics_iter = 0; topics_iter < topics_len; ++topics_iter) {
    metadata_requestRSEQ0string0a:
        if (used + 2 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_requestRSEQ0string0a;
        } else {
            if (fread(buff + used, 2, 1, f) != 1) return false;
            used += 2;
        }
        int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
    metadata_requestRSEQ0string0b:
        if (used + (s_len * 1) > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_requestRSEQ0string0b;
        } else {
            if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
            used += (s_len * 1);
        }
    }
    int ret = metadata_request_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void metadata_request_free(metadata_request *tgt)
{
    int32_t topics_iter = 0;
    for(topics_iter = 0; topics_iter < tgt->topics_len; ++topics_iter) {
        string_free(tgt->topics + topics_iter);
    }
    free(tgt->topics); tgt->topics = NULL;
}



typedef struct broker {
    int32_t node_id;
    string host;
    int32_t port;
} broker;

int broker_size(const broker const * b)
{
    int size = 8;
    size += string_size(&(b->host));
    return size;
}
int broker_pack(const broker const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->node_id); bytes_written += 4;
    bytes_written += string_pack(&(src->host), (tgt + bytes_written));
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->port); bytes_written += 4;

    return bytes_written;
}
int broker_unpack_new(broker *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->node_id = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    bytes_consumed += string_unpack_new(&(tgt->host), (src + bytes_consumed));
    tgt->port = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;

    return bytes_consumed;
}


int broker_write(const broker *src, FILE *f)
{
    int ret;
    size_t blk_size = broker_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!broker_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int broker_read_new(broker *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
brokerRSEQ0:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto brokerRSEQ0;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
brokerRSEQ1a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto brokerRSEQ1a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
brokerRSEQ1b:
    if (used + (s_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto brokerRSEQ1b;
    } else {
        if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
        used += (s_len * 1);
    }
    int ret = broker_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void broker_free(broker *tgt)
{
    string_free(&(tgt->host));
}



typedef struct partition_metadatum {
    int16_t partition_error_code;
    int32_t partition_id;
    int32_t leader;
    int32_t replicas_len;
    int32_t * replicas;
    int32_t isr_len;
    int32_t * isr;
} partition_metadatum;

int partition_metadatum_size(const partition_metadatum const * b)
{
    int size = 18;
    size += (b->replicas_len * 4);
    size += (b->isr_len * 4);
    return size;
}
int partition_metadatum_pack(const partition_metadatum const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = htobe16(src->partition_error_code); bytes_written += 2;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->partition_id); bytes_written += 4;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->leader); bytes_written += 4;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->replicas_len); bytes_written += 4;
    int32_t replicas_iter;
    for(replicas_iter = 0; replicas_iter < src->replicas_len; ++replicas_iter) {
        *((int32_t*)(tgt + bytes_written)) = htobe32(src->replicas[replicas_iter]); bytes_written += 4;
    }
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->isr_len); bytes_written += 4;
    int32_t isr_iter;
    for(isr_iter = 0; isr_iter < src->isr_len; ++isr_iter) {
        *((int32_t*)(tgt + bytes_written)) = htobe32(src->isr[isr_iter]); bytes_written += 4;
    }

    return bytes_written;
}
int partition_metadatum_unpack_new(partition_metadatum *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->partition_error_code = be16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    tgt->partition_id = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->leader = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    int32_t replicas_iter = 0;
    tgt->replicas_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->replicas = malloc(tgt->replicas_len * 4);
    if (tgt->replicas_len < 0) tgt->replicas_len = 0;
    for(replicas_iter = 0; replicas_iter < tgt->replicas_len; ++replicas_iter) {
        tgt->replicas[replicas_iter] = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    }
    int32_t isr_iter = 0;
    tgt->isr_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->isr = malloc(tgt->isr_len * 4);
    if (tgt->isr_len < 0) tgt->isr_len = 0;
    for(isr_iter = 0; isr_iter < tgt->isr_len; ++isr_iter) {
        tgt->isr[isr_iter] = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    }

    return bytes_consumed;
}


int partition_metadatum_write(const partition_metadatum *src, FILE *f)
{
    int ret;
    size_t blk_size = partition_metadatum_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!partition_metadatum_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int partition_metadatum_read_new(partition_metadatum *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
partition_metadatumRSEQ0:
    if (used + 10 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto partition_metadatumRSEQ0;
    } else {
        if (fread(buff + used, 10, 1, f) != 1) return false;
        used += 10;
    }
partition_metadatumRSEQ1a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto partition_metadatumRSEQ1a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t replicas_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
partition_metadatumRSEQ1b:
    if (used + (replicas_len * 4) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto partition_metadatumRSEQ1b;
    } else {
        if ((replicas_len * 4) && fread(buff + used, (replicas_len * 4), 1, f) != 1) return false;
        used += (replicas_len * 4);
    }
partition_metadatumRSEQ2a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto partition_metadatumRSEQ2a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t isr_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
partition_metadatumRSEQ2b:
    if (used + (isr_len * 4) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto partition_metadatumRSEQ2b;
    } else {
        if ((isr_len * 4) && fread(buff + used, (isr_len * 4), 1, f) != 1) return false;
        used += (isr_len * 4);
    }
    int ret = partition_metadatum_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void partition_metadatum_free(partition_metadatum *tgt)
{
    free(tgt->replicas); tgt->replicas = NULL;
    free(tgt->isr); tgt->isr = NULL;
}



typedef struct topic_metadatum {
    int16_t topic_error_code;
    string topic_name;
    int32_t partition_metadata_len;
    partition_metadatum * partition_metadata;
} topic_metadatum;

int topic_metadatum_size(const topic_metadatum const * b)
{
    int size = 6;
    size += string_size(&(b->topic_name));
    int32_t partition_metadata_iter = 0;
    for(partition_metadata_iter = 0; partition_metadata_iter < b->partition_metadata_len; ++partition_metadata_iter) {
        size += partition_metadatum_size(b->partition_metadata + partition_metadata_iter);
    }
    return size;
}
int topic_metadatum_pack(const topic_metadatum const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int16_t*)(tgt + bytes_written)) = htobe16(src->topic_error_code); bytes_written += 2;
    bytes_written += string_pack(&(src->topic_name), (tgt + bytes_written));
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->partition_metadata_len); bytes_written += 4;
    int32_t partition_metadata_iter;
    for(partition_metadata_iter = 0; partition_metadata_iter < src->partition_metadata_len; ++partition_metadata_iter) {
        bytes_written += partition_metadatum_pack(&(src->partition_metadata[partition_metadata_iter]), (tgt + bytes_written));
    }

    return bytes_written;
}
int topic_metadatum_unpack_new(topic_metadatum *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->topic_error_code = be16toh(* ((int16_t*)(src + bytes_consumed))); bytes_consumed += 2;
    bytes_consumed += string_unpack_new(&(tgt->topic_name), (src + bytes_consumed));
    int32_t partition_metadata_iter = 0;
    tgt->partition_metadata_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->partition_metadata = malloc(tgt->partition_metadata_len * sizeof(partition_metadatum));
    if (tgt->partition_metadata_len < 0) tgt->partition_metadata_len = 0;
    for(partition_metadata_iter = 0; partition_metadata_iter < tgt->partition_metadata_len; ++partition_metadata_iter) {
        bytes_consumed += partition_metadatum_unpack_new(&(tgt->partition_metadata[partition_metadata_iter]), (src + bytes_consumed));
    }

    return bytes_consumed;
}


int topic_metadatum_write(const topic_metadatum *src, FILE *f)
{
    int ret;
    size_t blk_size = topic_metadatum_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!topic_metadatum_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int topic_metadatum_read_new(topic_metadatum *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
topic_metadatumRSEQ0a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto topic_metadatumRSEQ0a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
topic_metadatumRSEQ0b:
    if (used + (s_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto topic_metadatumRSEQ0b;
    } else {
        if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
        used += (s_len * 1);
    }
topic_metadatumRSEQ1a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto topic_metadatumRSEQ1a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t partition_metadata_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
    int32_t partition_metadata_iter;
    for(partition_metadata_iter = 0; partition_metadata_iter < partition_metadata_len; ++partition_metadata_iter) {
    topic_metadatumRSEQ1partition_metadatum0:
        if (used + 10 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto topic_metadatumRSEQ1partition_metadatum0;
        } else {
            if (fread(buff + used, 10, 1, f) != 1) return false;
            used += 10;
        }
    topic_metadatumRSEQ1partition_metadatum1a:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto topic_metadatumRSEQ1partition_metadatum1a;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        int32_t replicas_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
    topic_metadatumRSEQ1partition_metadatum1b:
        if (used + (replicas_len * 4) > buff_len) {
            grow_buff(&buff, &buff_len);
            goto topic_metadatumRSEQ1partition_metadatum1b;
        } else {
            if ((replicas_len * 4) && fread(buff + used, (replicas_len * 4), 1, f) != 1) return false;
            used += (replicas_len * 4);
        }
    topic_metadatumRSEQ1partition_metadatum2a:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto topic_metadatumRSEQ1partition_metadatum2a;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        int32_t isr_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
    topic_metadatumRSEQ1partition_metadatum2b:
        if (used + (isr_len * 4) > buff_len) {
            grow_buff(&buff, &buff_len);
            goto topic_metadatumRSEQ1partition_metadatum2b;
        } else {
            if ((isr_len * 4) && fread(buff + used, (isr_len * 4), 1, f) != 1) return false;
            used += (isr_len * 4);
        }
    }
    int ret = topic_metadatum_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void topic_metadatum_free(topic_metadatum *tgt)
{
    string_free(&(tgt->topic_name));
    int32_t partition_metadata_iter = 0;
    for(partition_metadata_iter = 0; partition_metadata_iter < tgt->partition_metadata_len; ++partition_metadata_iter) {
        partition_metadatum_free(tgt->partition_metadata + partition_metadata_iter);
    }
    free(tgt->partition_metadata); tgt->partition_metadata = NULL;
}



typedef struct metadata_response {
    int32_t correlation_id;
    int32_t brokers_len;
    broker * brokers;
    int32_t topic_metadata_len;
    topic_metadatum * topic_metadata;
} metadata_response;

int metadata_response_size(const metadata_response const * b)
{
    int size = 12;
    int32_t brokers_iter = 0;
    for(brokers_iter = 0; brokers_iter < b->brokers_len; ++brokers_iter) {
        size += broker_size(b->brokers + brokers_iter);
    }
    int32_t topic_metadata_iter = 0;
    for(topic_metadata_iter = 0; topic_metadata_iter < b->topic_metadata_len; ++topic_metadata_iter) {
        size += topic_metadatum_size(b->topic_metadata + topic_metadata_iter);
    }
    return size;
}
int metadata_response_pack(const metadata_response const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->correlation_id); bytes_written += 4;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->brokers_len); bytes_written += 4;
    int32_t brokers_iter;
    for(brokers_iter = 0; brokers_iter < src->brokers_len; ++brokers_iter) {
        bytes_written += broker_pack(&(src->brokers[brokers_iter]), (tgt + bytes_written));
    }
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->topic_metadata_len); bytes_written += 4;
    int32_t topic_metadata_iter;
    for(topic_metadata_iter = 0; topic_metadata_iter < src->topic_metadata_len; ++topic_metadata_iter) {
        bytes_written += topic_metadatum_pack(&(src->topic_metadata[topic_metadata_iter]), (tgt + bytes_written));
    }

    return bytes_written;
}
int metadata_response_unpack_new(metadata_response *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->correlation_id = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    int32_t brokers_iter = 0;
    tgt->brokers_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->brokers = malloc(tgt->brokers_len * sizeof(broker));
    if (tgt->brokers_len < 0) tgt->brokers_len = 0;
    for(brokers_iter = 0; brokers_iter < tgt->brokers_len; ++brokers_iter) {
        bytes_consumed += broker_unpack_new(&(tgt->brokers[brokers_iter]), (src + bytes_consumed));
    }
    int32_t topic_metadata_iter = 0;
    tgt->topic_metadata_len = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->topic_metadata = malloc(tgt->topic_metadata_len * sizeof(topic_metadatum));
    if (tgt->topic_metadata_len < 0) tgt->topic_metadata_len = 0;
    for(topic_metadata_iter = 0; topic_metadata_iter < tgt->topic_metadata_len; ++topic_metadata_iter) {
        bytes_consumed += topic_metadatum_unpack_new(&(tgt->topic_metadata[topic_metadata_iter]), (src + bytes_consumed));
    }

    return bytes_consumed;
}


int metadata_response_write(const metadata_response *src, FILE *f)
{
    int ret;
    size_t blk_size = metadata_response_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!metadata_response_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int metadata_response_read_new(metadata_response *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
metadata_responseRSEQ0:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto metadata_responseRSEQ0;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
metadata_responseRSEQ1a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto metadata_responseRSEQ1a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t brokers_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
    int32_t brokers_iter;
    for(brokers_iter = 0; brokers_iter < brokers_len; ++brokers_iter) {
    metadata_responseRSEQ1broker0:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_responseRSEQ1broker0;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
    metadata_responseRSEQ1broker1a:
        if (used + 2 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_responseRSEQ1broker1a;
        } else {
            if (fread(buff + used, 2, 1, f) != 1) return false;
            used += 2;
        }
        int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
    metadata_responseRSEQ1broker1b:
        if (used + (s_len * 1) > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_responseRSEQ1broker1b;
        } else {
            if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
            used += (s_len * 1);
        }
    }
metadata_responseRSEQ2a:
    if (used + 4 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto metadata_responseRSEQ2a;
    } else {
        if (fread(buff + used, 4, 1, f) != 1) return false;
        used += 4;
    }
    int32_t topic_metadata_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
    int32_t topic_metadata_iter;
    for(topic_metadata_iter = 0; topic_metadata_iter < topic_metadata_len; ++topic_metadata_iter) {
    metadata_responseRSEQ2topic_metadatum0a:
        if (used + 2 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_responseRSEQ2topic_metadatum0a;
        } else {
            if (fread(buff + used, 2, 1, f) != 1) return false;
            used += 2;
        }
        int16_t s_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
    metadata_responseRSEQ2topic_metadatum0b:
        if (used + (s_len * 1) > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_responseRSEQ2topic_metadatum0b;
        } else {
            if ((s_len * 1) && fread(buff + used, (s_len * 1), 1, f) != 1) return false;
            used += (s_len * 1);
        }
    metadata_responseRSEQ2topic_metadatum1a:
        if (used + 4 > buff_len) {
            grow_buff(&buff, &buff_len);
            goto metadata_responseRSEQ2topic_metadatum1a;
        } else {
            if (fread(buff + used, 4, 1, f) != 1) return false;
            used += 4;
        }
        int32_t partition_metadata_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
        int32_t partition_metadata_iter;
        for(partition_metadata_iter = 0; partition_metadata_iter < partition_metadata_len; ++partition_metadata_iter) {
        metadata_responseRSEQ2topic_metadatum1partition_metadatum0:
            if (used + 10 > buff_len) {
                grow_buff(&buff, &buff_len);
                goto metadata_responseRSEQ2topic_metadatum1partition_metadatum0;
            } else {
                if (fread(buff + used, 10, 1, f) != 1) return false;
                used += 10;
            }
        metadata_responseRSEQ2topic_metadatum1partition_metadatum1a:
            if (used + 4 > buff_len) {
                grow_buff(&buff, &buff_len);
                goto metadata_responseRSEQ2topic_metadatum1partition_metadatum1a;
            } else {
                if (fread(buff + used, 4, 1, f) != 1) return false;
                used += 4;
            }
            int32_t replicas_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
        metadata_responseRSEQ2topic_metadatum1partition_metadatum1b:
            if (used + (replicas_len * 4) > buff_len) {
                grow_buff(&buff, &buff_len);
                goto metadata_responseRSEQ2topic_metadatum1partition_metadatum1b;
            } else {
                if ((replicas_len * 4) && fread(buff + used, (replicas_len * 4), 1, f) != 1) return false;
                used += (replicas_len * 4);
            }
        metadata_responseRSEQ2topic_metadatum1partition_metadatum2a:
            if (used + 4 > buff_len) {
                grow_buff(&buff, &buff_len);
                goto metadata_responseRSEQ2topic_metadatum1partition_metadatum2a;
            } else {
                if (fread(buff + used, 4, 1, f) != 1) return false;
                used += 4;
            }
            int32_t isr_len = be32toh(*( (( int32_t *) (buff + used)) - 1));
        metadata_responseRSEQ2topic_metadatum1partition_metadatum2b:
            if (used + (isr_len * 4) > buff_len) {
                grow_buff(&buff, &buff_len);
                goto metadata_responseRSEQ2topic_metadatum1partition_metadatum2b;
            } else {
                if ((isr_len * 4) && fread(buff + used, (isr_len * 4), 1, f) != 1) return false;
                used += (isr_len * 4);
            }
        }
    }
    int ret = metadata_response_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void metadata_response_free(metadata_response *tgt)
{
    int32_t brokers_iter = 0;
    for(brokers_iter = 0; brokers_iter < tgt->brokers_len; ++brokers_iter) {
        broker_free(tgt->brokers + brokers_iter);
    }
    free(tgt->brokers); tgt->brokers = NULL;
    int32_t topic_metadata_iter = 0;
    for(topic_metadata_iter = 0; topic_metadata_iter < tgt->topic_metadata_len; ++topic_metadata_iter) {
        topic_metadatum_free(tgt->topic_metadata + topic_metadata_iter);
    }
    free(tgt->topic_metadata); tgt->topic_metadata = NULL;
}



typedef struct message {
    int64_t offset;
    int32_t message_size;
    int32_t crc;
    int8_t magic_byte;
    int8_t attributes;
    bytes key;
    bytes value;
} message;

int message_size(const message const * b)
{
    int size = 18;
    size += bytes_size(&(b->key));
    size += bytes_size(&(b->value));
    return size;
}
int message_pack(const message const *src, char *tgt)
{
    size_t bytes_written = 0;
    *((int64_t*)(tgt + bytes_written)) = htobe64(src->offset); bytes_written += 8;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->message_size); bytes_written += 4;
    *((int32_t*)(tgt + bytes_written)) = htobe32(src->crc); bytes_written += 4;
    *((int8_t*)(tgt + bytes_written)) = (src->magic_byte); bytes_written += 1;
    *((int8_t*)(tgt + bytes_written)) = (src->attributes); bytes_written += 1;
    bytes_written += bytes_pack(&(src->key), (tgt + bytes_written));
    bytes_written += bytes_pack(&(src->value), (tgt + bytes_written));

    return bytes_written;
}
int message_unpack_new(message *tgt, const char const *src)
{
    size_t bytes_consumed = 0;
    tgt->offset = be64toh(* ((int64_t*)(src + bytes_consumed))); bytes_consumed += 8;
    tgt->message_size = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->crc = be32toh(* ((int32_t*)(src + bytes_consumed))); bytes_consumed += 4;
    tgt->magic_byte = (* ((int8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    tgt->attributes = (* ((int8_t*)(src + bytes_consumed))); bytes_consumed += 1;
    bytes_consumed += bytes_unpack_new(&(tgt->key), (src + bytes_consumed));
    bytes_consumed += bytes_unpack_new(&(tgt->value), (src + bytes_consumed));

    return bytes_consumed;
}


int message_write(const message *src, FILE *f)
{
    int ret;
    size_t blk_size = message_size(src);
    char * buff = (char*) malloc(blk_size);
    if(!message_pack(src, buff)) return false;
    ret = fwrite(buff, blk_size, 1, f);
    free(buff);
    return ret;
}
int message_read_new(message *tgt, FILE *f)
{
    size_t buff_len = 1024;
    size_t used = 0;
    char *buff = malloc(buff_len);
messageRSEQ0a:
    if (used + 2 > buff_len) {
        grow_buff(&buff, &buff_len);
        goto messageRSEQ0a;
    } else {
        if (fread(buff + used, 2, 1, f) != 1) return false;
        used += 2;
    }
    int16_t b_len = be16toh(*( (( int16_t *) (buff + used)) - 1));
messageRSEQ0b:
    if (used + (b_len * 1) > buff_len) {
        grow_buff(&buff, &buff_len);
        goto messageRSEQ0b;
    } else {
        if ((b_len * 1) && fread(buff + used, (b_len * 1), 1, f) != 1) return false;
        used += (b_len * 1);
    }
    int ret = message_unpack_new(tgt, buff);
    free(buff);
    return ret;
}
void message_free(message *tgt)
{
    bytes_free(&(tgt->key));
    bytes_free(&(tgt->value));
}




#endif
