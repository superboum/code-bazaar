#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>

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
  printf("ERROR. %s", reason);
  exit(1);
}

int main(int argc, char** argv) {
  int udp_sock_fd;
  struct sockaddr_in my_addr;
  my_addr.sin_family = AF_INET;
  my_addr.sin_port = 1337;

  if ((udp_sock_fd = socket(AF_INET, SOCK_DGRAM, 0)) == -1)
    handle_error("Unable to create socket");

  //if (bind(udp_sock_fd, 
  return 0;
}
