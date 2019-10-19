#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h> 
#include <sys/types.h> 
#include <netdb.h> 
#include <netinet/in.h> 
#include <unistd.h>
#include <strings.h>
#include <string.h>

int main(int argc, char* argv[]) {
  int sockfd, connfd, pid, err, enable; 
  socklen_t len;
  struct sockaddr_in servaddr, client; 

  char *name = "inconnu";
  if (argc >= 2) name = argv[1];
  
  char http_content[256];
  sprintf(http_content, "<h1>salut %s</h1>\r\n", name);
  size_t http_content_len = strlen(http_content);

  char http_headers[256];
  sprintf(http_headers, "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length:%ld\r\nConnection: Close\r\n\r\n", http_content_len);
  size_t http_headers_len = strlen(http_headers);


  sockfd = socket(AF_INET, SOCK_STREAM, 0); 
  if (sockfd == -1) { 
    perror("socket creation failed..."); 
    exit(EXIT_FAILURE); 
  } 

  enable=1;
  err = setsockopt (sockfd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int));
  if (err < 0) {
    perror("Error setting socket to SO_REUSEADDR");
    exit(EXIT_FAILURE);
  }

  bzero(&servaddr, sizeof(servaddr)); 
  
  servaddr.sin_family = AF_INET; 
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY); 
  servaddr.sin_port = htons(8080); 
  
  if ((bind(sockfd, (struct sockaddr*)&servaddr, sizeof(servaddr))) != 0) { 
    perror("socket bind failed..."); 
    exit(EXIT_FAILURE); 
  } 
  
  if ((listen(sockfd, SOMAXCONN)) != 0) { 
    perror("Listen failed..."); 
    exit(EXIT_FAILURE); 
  } 
  
  len = sizeof(client); 
  
  for (;;) {
    connfd = accept(sockfd, (struct sockaddr*)&client, &len); 
    if (connfd < 0) { 
      perror("server acccept failed..."); 
      continue;
    }

    if ((pid = fork()) == -1) {
      close(connfd);
      perror("unable to accept new connection due to failed fork");
    } else if (pid > 0) {
      // parent here
      close(connfd);
      continue;
    } else if (pid == 0) {
      // child here
      send(connfd, http_headers, http_headers_len, 0);
      send(connfd, http_content, http_content_len, 0);

      close(connfd);
      exit(EXIT_SUCCESS);
    }
  }
  
  close(sockfd); 
  return EXIT_SUCCESS;
}
