FROM ubuntu:latest AS builder
RUN apt-get update && apt-get install -y build-essential
COPY . /opt
WORKDIR /opt
RUN gcc -Wall -static main.c -o main

FROM scratch
COPY --from=builder /opt/main /server
ENTRYPOINT ["/server"]
