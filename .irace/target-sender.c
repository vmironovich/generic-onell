#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define BUFFER_SIZE 10240

char send_buffer[BUFFER_SIZE + 1];
char recv_buffer[BUFFER_SIZE + 1];

int main(int argc, char *argv[]) {
    int i, sum_length = -1, port_number = -1;
    int socket_id;
    char *send_buffer_ptr = send_buffer;
    struct sockaddr_in address;
    int num_bytes_read, num_bytes_written;
    int buffer_size = BUFFER_SIZE;

    for (i = 1; i < argc; ++i) {
        sum_length += 1 + strlen(argv[i]);
    }
    if (sum_length > BUFFER_SIZE) {
        printf("Command length too long: %d\n", sum_length);
        return 1;
    }
    for (i = 1; i < argc; ++i) {
        if (!strcmp("--port", argv[i]) && i + 1 < argc) {
            port_number = atoi(argv[i + 1]);
        }
        int length = strlen(argv[i]);
        memcpy(send_buffer_ptr, argv[i], length + 1);
        if (i + 1 < argc) {
            send_buffer_ptr[length] = ' ';
        }
        send_buffer_ptr += length + 1;
    }

    if (port_number <= 0) {
        printf("Port number (--port) was not found or is incorrect: %d\n", port_number);
        return 1;
    }

    socket_id = socket(AF_INET, SOCK_DGRAM, 0);
    if (socket_id < 0) {
        int error = errno;
        printf("Could not open socket! Error code: %d (%s)\n", error, strerror(error));
        return 1;
    }

    if (setsockopt(socket_id, SOL_SOCKET, SO_SNDBUF, &buffer_size, sizeof(buffer_size)) == -1) {
        int error = errno;
        printf("Could not set socket's send buffer size! Error code: %d (%s)\n", error, strerror(error));
        return 1;
    }

    if (setsockopt(socket_id, SOL_SOCKET, SO_RCVBUF, &buffer_size, sizeof(buffer_size)) == -1) {
        int error = errno;
        printf("Could not set socket's receive buffer size! Error code: %d (%s)\n", error, strerror(error));
        return 1;
    }

    address.sin_family = AF_INET;
    address.sin_port = htons((unsigned) port_number);
    address.sin_addr.s_addr = INADDR_ANY; // local connections only, sort of
    if (connect(socket_id, (struct sockaddr *) &address, sizeof(address))) {
        int error = errno;
        printf("Could not connect socket! Error code: %d (%s)\n", error, strerror(error));
        close(socket_id);
        return 1;
    }

    num_bytes_written = write(socket_id, send_buffer, sum_length);
    if (num_bytes_written != sum_length) {
        if (num_bytes_written < 0) {
            int error = errno;
            printf("Could not write to the socket! Error code: %d (%s)\n", error, strerror(error));
        } else {
            printf("Wrote %d bytes instead of %d, exiting\n", num_bytes_written, sum_length);
        }
        close(socket_id);
        return 1;
    }

    num_bytes_read = read(socket_id, recv_buffer, BUFFER_SIZE);
    if (num_bytes_read < 0) {
        int error = errno;
        printf("Could not read from the socket! Error code: %d (%s)\n", error, strerror(error));
        close(socket_id);
    }
    recv_buffer[num_bytes_read] = 0;
    printf("%s\n", recv_buffer);
    close(socket_id);

    return 0;
}
