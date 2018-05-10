#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/ip.h>

#define LISTEN_BACKGLOG 50

typedef struct {
  uint16_t node;
  uint16_t counter;
  uint16_t ttl;
} msg_structured_t;

typedef union {
  msg_structured_t structured;
  char buffer[6];
} msg_t;

void handle_error(char* reason) {
  printf("ERROR. %s\n", reason);
  exit(1);
}

int main(int argc, char** argv) {
  int udp_sock_fd;
  struct sockaddr_in my_addr;
  msg_t recv_msg;
  int read_bytes;

  /* @FIXME use getaddrinfo */
  /* @NOTE No need to accept or listen in UDP. socket() bind() recv() sendto() */
  memset(&my_addr, 0, sizeof(struct sockaddr_in));
  my_addr.sin_family = AF_INET;
  my_addr.sin_port = 1337;
  if (inet_pton(AF_INET, "0.0.0.0", &my_addr.sin_addr.s_addr) != 1)
    handle_error("Unable to set ip address 0.0.0.0");

  if ((udp_sock_fd = socket(AF_INET, SOCK_DGRAM, 0)) == -1)
    handle_error("Unable to create socket");

  if (bind(udp_sock_fd, (struct sockaddr*) &my_addr, sizeof(struct sockaddr_in)) == -1)
    handle_error("Unable to bind socket");

  read_bytes = recv(udp_sock_fd, &recv_msg.buffer, sizeof(msg_t), MSG_WAITALL);
  printf("%d %d %d\n", recv_msg.structured.node, recv_msg.structured.counter, recv_msg.structured.ttl);

  return 0;
}
