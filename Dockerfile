FROM alpine:latest

RUN apk --no-cache add \
    clang \
    llvm16 \
    llvm16-dev \
    flex \
    bison \
    make \
    g++

ENV LLVMCONFIG /usr/bin/llvm-config
ENV CXX /usr/bin/clang++


WORKDIR /usr/src/myapp
COPY Makefile lexer.hpp lexer.l parser.y ast.cpp ast.hpp symbol.cpp ./

RUN make


