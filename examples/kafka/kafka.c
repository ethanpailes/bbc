
#include <stdlib.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>


#include "kafka-types.bb.h"


#define KAFKA_LOCATION "localhost"
#define KAFKA_PORT "9092"
#define KAFKA_TOPIC "byte-blocks-example"
#define CORRELATION_ID 1789

#define METADATA_REQUEST 3
#define PRODUCE_REQUEST 0
#define FETCH_REQUEST 1

#define KAFKA_STR( str ) { strlen( (str) ), (str) }

const string client_id = KAFKA_STR("byte-blocks");
string kafka_topic = KAFKA_STR( KAFKA_TOPIC );

void print_kstr(FILE* fp, string kstr)
{
        char *cstr = malloc(kstr.s_len + 1);
        memcpy(cstr, kstr.s, kstr.s_len);
        cstr[kstr.s_len] = '\0';
        fprintf(fp, cstr);
        free(cstr);
}

FILE * open_socket(const char *address, const char *port)
{
        int status, sock;
        struct addrinfo hints;
        struct addrinfo *res;

        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_UNSPEC;     // don't care IPv4 or IPv6
        hints.ai_socktype = SOCK_STREAM; // TCP stream sockets

        if ((status = getaddrinfo(address, port, &hints, &res)) != 0) {
            fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(status));
            exit(1);
        }

        sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);


        if ((status = connect(sock, res->ai_addr, res->ai_addrlen)) == -1) {
                fprintf(stderr, "connect failure!\n");
                exit(1);
        }

        freeaddrinfo(res);

        FILE *fp = fdopen(sock, "r+");
        if (fp == NULL) {
                fprintf(stderr, "Error creating file pointer.");
                exit(1);
        }
        return fp;
}


void place_metadata_request(FILE * sock)
{
        /* The request header */
        request_message_hdr req_header;
        req_header.api_key = METADATA_REQUEST;
        req_header.correlation_id = CORRELATION_ID;
        req_header.client_id = client_id;

        /* A singleton metadata_request */
        metadata_request mr;
        mr.topics_len = 1;
        mr.topics = &kafka_topic;

        /* pack the message in a length prefixing wrapper */
        wrapper w;
        w.msg_len = metadata_request_size(&mr)
                        + request_message_hdr_size(&req_header);
        w.msg = malloc(w.msg_len);
        request_message_hdr_pack(&req_header, w.msg);


        metadata_request_pack(&mr, w.msg + request_message_hdr_size(&req_header));

        wrapper_write(&w, sock);

        /* clean up */
        free(w.msg);
}

void print_broker(broker * b)
{
        printf("  node_id = %d\n", b->node_id);
        printf("  host = ");
        print_kstr(stdout, b->host);
        printf("\n");
        printf("  port = %d\n", b->port);
}


void print_partition_metadatum(partition_metadatum * p)
{
        printf("    partition_error_code = %d.\n", p->partition_error_code);
        printf("    partition_id = %d.\n", p->partition_id);
        printf("    leader = %d.\n", p->leader);
        printf("    replicas_len = %d.\n", p->replicas_len);
        printf("    isr_len = %d.\n", p->isr_len);
}


void print_topic_metadatum(topic_metadatum * t)
{
        int i;

        printf("  topic_error_code = %d\n", t->topic_error_code);        
        printf("  topic_name = ", t->topic_error_code);        
        print_kstr(stdout, t->topic_name);
        printf("\n");

        printf("  Partion Metadata[%d] as follows:\n",
                                        t->partition_metadata_len);
        for (i = 0; i < t->partition_metadata_len; ++i) {
                print_partition_metadatum(t->partition_metadata + i);
        }
}

void read_and_print_metadata(FILE * sock)
{
        int i;

        wrapper w;
        wrapper_read_new(&w, sock);

        metadata_response res;
        metadata_response_unpack_new(&res, w.msg);

        printf("Correlation ID = %d. Expect %d\n",
                        res.correlation_id, CORRELATION_ID);
        printf("Brokers[%d] as follows:\n", res.brokers_len);

        for (i = 0; i < res.brokers_len; ++i) {
                print_broker(&(res.brokers[i]));
        }

        printf("Topic Metadata[%d] as follows:\n", res.topic_metadata_len);

        for (i = 0; i < res.topic_metadata_len; ++i) {
                print_topic_metadatum(&(res.topic_metadata[i]));
        }

        wrapper_free(&w);
        metadata_response_free(&res);
}


int main(int argc, char *argv[])
{
        FILE * sock = open_socket(KAFKA_LOCATION, KAFKA_PORT);

        printf("CLUSTER METADATA:\n\n");

        place_metadata_request(sock);
        read_and_print_metadata(sock);

        fclose(sock);
        return 0;
}
