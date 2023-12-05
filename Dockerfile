FROM alpine:latest

RUN apk --no-cache add \
    clang \
    llvm16-dev \
    flex \
    bison \
    make \
    g++

WORKDIR /usr/src/myapp

COPY . .

RUN make


